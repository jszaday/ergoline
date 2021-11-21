package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.UnparseAst
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

    override def toString: String = {
      successors.map(_.toString).mkString("\n")
    }
  }

  abstract class ScopingConstruct extends Construct {
    def body: Option[Construct]
    def label: String
    override def name: String = body.map(_.name).getOrElse(s"dummy_${this.id}")
    override def toString: String = {
      s"subgraph cluster_${this.id} {" + {
        s"label=\" $ { this.label } \ ";\n" +
          body.map(_.toString).getOrElse("")
      } + "}\n" + super.toString
    }
  }

  case class Clause(var node: EirSdagWhen, var body: Option[Construct])
      extends ScopingConstruct {
    override def label: String = UnparseAst.visitWhenHeader(node) + " ;"
  }

  case class SerialBlock(var slst: List[EirNode]) extends Construct {
    override def toString: String = {
      this.id.toString + "[label=\"" + {
        slst.map(_.toString).map(UnparseAst.escape).mkString("\\n")
      } + "\"];\n" + super.toString
    }
  }

  case class Loop(var node: EirNode, var body: Option[Construct])
      extends ScopingConstruct {
    override def label: String = UnparseAst.visitLoopHeader(node).format(";")
  }

  def toGraph(construct: Construct): String = {
    val edges = new ListBuffer[String]

    def makeEdge(f: Construct, t: Construct): String = {
      s"${f.id} -> ${t.name} [ " + {
        t match {
          case s: ScopingConstruct => s"lhead=cluster_${s.id} "
          case _                   => ""
        }
      } + "minlen=2 ]"
    }

    def findLast(f: Construct): List[Construct] = {
      if (f.successors.isEmpty) {
        List(f)
      } else {
        f.successors.partition(_.successors.isEmpty) match {
          case (_1, _2) => _1 ++ _2.flatMap(findLast)
        }
      }
    }

    def enumerate(f: Construct): Unit = {
      f match {
        case s: ScopingConstruct =>
          val last = s.body.map(findLast).getOrElse(Nil)
          assert(s.body.isEmpty || last.nonEmpty)
          last.foreach(l =>
            f.successors.foreach(t => edges.append(makeEdge(l, t)))
          )
          s.body.foreach(enumerate)
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
          x.children.foldLeft(head)((acc, y) => {
            val next = visit(y, acc)
            head = head.orElse(next)
            putSuccessor(acc, next)
          })
          head
        case x: EirForLoop     => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirDoWhileLoop => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirWhileLoop   => Some(Loop(x, x.body.flatMap(visit(_, None))))
        case x: EirSdagWhen    => Some(Clause(x, x.body.flatMap(visit(_, None))))
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

    override def annotations: Seq[String] = Seq("main")

    override def apply(n: EirNode): Unit = {
      val asClass = assertValid[EirClass](n)
      asClass.members
        .map(_.member)
        .collect { case f: EirFunction if f.body.nonEmpty => f }
        .flatMap(Analysis.analyze)
        .map(toGraph)
        .foreach(println(_))
    }
  }
}
