package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.Processes.RichProcessesSyntax.RichSeq
import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid

import scala.collection.mutable.ListBuffer

object Segmentation {
  private[this] var _count: Int = 0

  sealed abstract class Construct {
    var id: Int = {
      _count += 1
      _count
    }

    var successors: List[Construct] = Nil

    def name: String = id.toString

    def head: String = this.name

    def tail: Seq[Construct] = Seq(this)

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

  case class Clause(var node: EirNode, var body: Option[Construct])
      extends ScopingConstruct {
    override def label: String = {
      node match {
        case x: EirSdagWhen => UnparseAst.visitWhenHeader(x) + " ;"
        case x: EirAwait    => UnparseAst.visit(x) + ";"
        case _              => ???
      }
    }
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
    override def toString: String = {
      this.id.toString + "[label=\"" + UnparseAst.forceSemi {
        slst
          .map(_.toString)
          .map(UnparseAst.escape)
          .mkString("\\n")
          .replace("\\n", "\\l")
      } + "\\l\"];\n" + super.toString
    }
  }

  case class Loop(var node: EirNode, var body: Option[Construct])
      extends ScopingConstruct {
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

  def toGraph(construct: Construct): String = {
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

    "digraph g {" + {
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
      from.filterNot(to.contains(_)).foreach(_.successors ++= to)
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
        case x: EirAwait       => Some(Clause(x, None))
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
    override def phase: Phase = Phase.Load

    override def after: Seq[Pass] = Seq()

    override def apply(node: EirNode): Unit = {
      node match {
        case x: EirNamespace => x.children.foreach(apply)
        case x: EirClassLike => x.members
            .filter(_.hasAnnotation("threaded"))
            .map(_.member)
            .collect { case f: EirFunction if f.body.nonEmpty => f }
            .flatMap(Analysis.analyze)
            .map(toGraph)
            .foreach(println(_))
        case _ => ;
      }
    }
  }
}
