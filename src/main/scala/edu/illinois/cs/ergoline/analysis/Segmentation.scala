package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichSeq
import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.{globals, passes}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Segmentation {
  private[this] var _count: Int = 0

  sealed abstract class Construct {
    private[this] var _depth: Int = 0

    val id: Int = {
      _count += 1
      _count
    }

    var predecessors: List[Construct] = Nil
    var successors: List[Construct] = Nil

    def name: String = id.toString

    def head: String = this.name

    def tail: Seq[Construct] = Seq(this)

    def declarations: List[EirNode] = Nil

    def encapsulate: Boolean = false

    def depth: Int = this._depth
    def depth_=(nu: Int): Unit = {
      val offset = if (this.declarations.nonEmpty) 1 else 0
      val unwind = if (this.encapsulate) offset else 0
      this._depth = nu + offset
      this.successors.foreach(_.depth = nu + offset - unwind)
    }

    override def toString: String = {
      successors.map(_.toString).mkString("\n")
    }
  }

  abstract class ScopingConstruct extends Construct {
    def members: Iterable[Construct]
    def label: String
    def divergent: Boolean = true

    override def name: String =
      members.headOption.map(_.name).getOrElse(s"dummy_${this.id}")

    override def tail: Seq[Construct] =
      this.members.flatMap(findLast).flatMap(_.tail).toSeq

    override def depth_=(nu: Int): Unit = {
      super.depth_=(nu)
      this.members.foreach(_.depth = this.depth)
    }

    override def encapsulate: Boolean = true

    override def toString: String = {
      s"subgraph cluster_${this.id} {" + {
        "label=\"" + this.label + "\";\n" + {
          if (members.isEmpty) {
            s"dummy_${this.id} [shape=point style=invis];\n"
          } else {
            members.map(_.toString).mkString("\n")
          }
        }
      } + "}\n" + super.toString
    }
  }

  case class Clause(var node: EirSdagWhen, var body: Option[Construct])
      extends ScopingConstruct {
    override def label: String = {
      UnparseAst.visitWhenHeader(this.node) + " ;"
    }
    override def declarations: List[EirNode] = node.declarations
    override def members: Iterable[Construct] = body.toIterable
  }

  case class MultiClause(var node: EirAwaitMany, var members: Seq[Clause])
      extends ScopingConstruct {
    override def divergent: Boolean = !node.waitAll
    override def label: String = s"await ${if (node.waitAll) "all" else "any"};"
  }

  case class Divergence(var node: EirNode, var members: Seq[Construct])
      extends ScopingConstruct {
    override def label: String = {
      node match {
        case x: EirIfElse => s"if (${x.test}); else;"
        case x: EirMatch  => s"match ${x.expression};"
        case _            => ???
      }
    }
  }

  case class SerialBlock(var slst: List[EirNode]) extends Construct {
    var threaded: Boolean = false

    override def toString: String = {
      this.id.toString + "[label=\"" + UnparseAst.forceSemi {
        slst
          .map(_.toString)
          .map(UnparseAst.escape)
          .mkString("\\n")
          .replace("\\n", "\\l")
      } + "\\l\"];\n" + super.toString
    }

    // TODO ( enhance discovery of declarations )
    override def declarations: List[EirNode] = {
      slst.collect {
        case x: EirDeclaration      => List(x)
        case x: EirMultiDeclaration => x.children
      }.flatten
    }
  }

  case class Loop(var node: EirNode, var body: Option[Construct])
      extends ScopingConstruct {

    val iteratorDeclaration: Option[EirDeclaration] = {
      node match {
        case x: EirForLoop => x.header match {
            case y: EirForAllHeader => y.expression.foundType.map(t =>
                EirDeclaration(
                  None,
                  isFinal = true,
                  "__it__",
                  t,
                  Some(y.expression)
                )
              )
            case _ => None
          }
        case _ => None
      }
    }

    override def declarations: List[EirNode] = {
      node match {
        case x: EirForLoop =>
          (x.header match {
            case y: EirForAllHeader => y.declaration
            case y: EirCStyleHeader => y.declaration
          }).toList ++ iteratorDeclaration.toList
        case _ => Nil
      }
    }

    override def label: String = {
      val (head, tail) = UnparseAst.visitLoopHeader(node)
      s"$head;$tail"
    }

    override def members: Iterable[Construct] = body.toIterable
  }

  private def findLast(f: Construct): List[Construct] = {
    if (f.successors.isEmpty) {
      List(f)
    } else {
      f.successors.partition(_.successors.isEmpty) match {
        case (_1, _2) => _1 ++ _2.flatMap(findLast)
      }
    }
  }

  def toGraph(construct: Construct, name: String): String = {
    val edges = new ListBuffer[String]

    def makeEdge(f: Construct, t: Construct, style: String = ""): String = {
      s"${f.name} -> ${t.name} [ " + {
        t match {
          case s: ScopingConstruct => s"lhead=cluster_${s.id} "
          case _                   => ""
        }
      } + s"minlen=2 $style ]"
    }

    def enumerate(f: Construct): Unit = {
      f match {
        case s: ScopingConstruct =>
          var last: Iterable[Construct] = s.tail
          if (!s.divergent) last = last.lastOption
          assert(s.members.isEmpty || last.nonEmpty)
          last.foreach(l =>
            f.successors.foreach(t => edges.append(makeEdge(l, t)))
          )
          if (s.members.size >= 2) {
            s.members.tail.foldLeft(s.members.headOption)((left, right) => {
              left.foreach(l => edges.append(makeEdge(l, right, "style=invis")))
              Some(right)
            })
          }
          s.members.foreach(enumerate)
        case _ => f.successors.foreach(t => edges.append(makeEdge(f, t)))
      }
      f.successors.foreach(enumerate)
    }

    enumerate(construct)

    s"digraph $name {" + {
      "graph [compound=true];\n" +
        construct.toString + "\n" +
        edges.mkString("\n")
    } + "}"
  }

  def containsWhen(node: EirNode): Boolean = {
    node.isInstanceOf[EirSdagWhen] || Find
      .descendant(
        node,
        {
          // TODO temporarily forbid SDAG within...
          case _: EirLambdaExpression => None
          case _: EirFunction         => None
          // AwaitMany will always contain...
          case _: EirSdagWhen => Some(true)
          case _: EirAwait    => Some(true)
          case _              => Some(false)
        }
      )
      .nonEmpty
  }

  object Analysis {
    def analyze(fn: EirFunction): Option[Construct] = {
      fn.body.flatMap(visit(_, None))
    }

    def getBlock(prev: Option[Construct]): SerialBlock = {
      prev.collect { case b: SerialBlock => b }.getOrElse(SerialBlock(Nil))
    }

    def putSuccessor(
        from: Option[Construct],
        to: Option[Construct]
    ): Option[Construct] = {
      from
        .filterNot(to.contains(_))
        .foreach(x => {
          x.successors ++= to
          to.foreach(_.predecessors :+= x)
        })
      to
    }

    def visit(node: EirNode, prev: Option[Construct]): Option[Construct] = {
      node match {
        case x: EirBlock =>
          var head: Option[Construct] = None
          val split = x.children.orderedPartition(containsWhen)
          split.foldLeft(head)((sum, group) => {
            group match {
              case (false, nodes) =>
                val block = Some(SerialBlock(Nil))
                head = head.orElse(block)
                putSuccessor(sum, block)
                block.foreach(_.slst ++= nodes)
                block
              case (true, nodes) => nodes.foldLeft(sum)((partialSum, node) => {
                  val next = visit(node, None)
                  next.foreach {
                    case x: SerialBlock => x.threaded = true
                    case _              =>
                  }
                  head = head.orElse(next)
                  putSuccessor(partialSum, next)
                })
            }
          })
          head
        case x: EirForLoop     => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirDoWhileLoop => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirWhileLoop   => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirSdagWhen    => Some(Clause(x, x.body.flatMap(visit(_, None))))
        case x: EirAwaitMany => Some(
            MultiClause(
              x, {
                x.children.flatMap(visit(_, None)).collect { case c: Clause =>
                  c
                }
              }
            )
          )
        case x: EirIfElse => Some(
            Divergence(
              x, {
                (x.ifTrue.flatMap(visit(_, None)) ++ x.ifFalse.flatMap(
                  visit(_, None)
                )).toSeq
              }
            )
          )
        case x: EirMatch => Some(
            Divergence(
              x, {
                x.cases.flatMap(visit(_, None))
              }
            )
          )
        case x: EirMatchCase => x.body.flatMap(visit(_, prev))
        case _ =>
          val block = getBlock(prev)
          block.slst :+= node
          Some(block)
      }
    }
  }

  class Pass extends passes.Pass {
    private val _memo: mutable.Map[EirFunction, Construct] = mutable.Map()

    override def phase: Phase = Phase.Load

    override def after: Seq[Pass] = Seq()

    def apply(fn: EirFunction): Option[Construct] = {
      this._memo.get(fn).orElse {
        val cons = Analysis.analyze(fn)
        cons.foreach(this._memo.put(fn, _))
        cons.foreach(_.depth = if (fn.functionArgs.isEmpty) 0 else 1)
        cons
      }
    }

    override def apply(node: EirNode): Unit = {
      node match {
        case x: EirNamespace => x.children.foreach(apply)
        case x: EirClassLike =>
          val fns = x.members
            .filter(_.hasAnnotation("threaded"))
            .map(_.member)
            .collect { case f: EirFunction if f.body.nonEmpty => f }
          val res = fns.map(fn => (fn, apply(fn))).collect {
            case (fn, Some(cons)) => (fn, cons)
          }
          if (globals.verbose) {
            res.foreach { case (fn, cons) => println(toGraph(cons, fn.name)) }
          }
        case x: EirFunction => this.apply(x)
        case _              => ;
      }
    }
  }
}
