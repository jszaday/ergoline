package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast.{
  EirBlock,
  EirCStyleHeader,
  EirClass,
  EirExpressionNode,
  EirForLoop,
  EirFunction,
  EirIfElse,
  EirNode,
  EirReturn,
  EirScope
}
import edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.util.assertValid

import scala.collection.mutable

object ControlFlow {
  case class Edge(from: Node, to: Node) {
    var label: Option[String] = None
    override def toString: String = s"${from.id} -> ${to.id}" + label
      .map(" [label=\"" + _ + "\"]")
      .getOrElse("")
  }

  case class Node(
      id: String,
      label: Option[String] = None,
      shape: Option[String] = None
  ) {
    val in: mutable.Set[Edge] = mutable.Set[Edge]()
    val out: mutable.Set[Edge] = mutable.Set[Edge]()
    var statements: List[EirNode] = Nil
    var locked: Boolean = false

    def dotLabel: String = {
      val lt =
        if (this.statements.isEmpty) {
          id
        } else {
          statements
            .map(_.toString)
            .map(_.replace("\"", "\\\""))
            .mkString("\\n")
        }
      s"""label="$lt"""" + (label match {
        case Some(l) => s""", xlabel="$l""""
        case None    => ""
      })
    }
  }

  class Graph {
    val start: Node = Node("start")
    val end: Node = Node("end")
    val nodes: mutable.Set[Node] = mutable.Set[Node](start, end)
    private[this] var _nodeCount = 0

    def nextNode: String = {
      _nodeCount += 1
      _nodeCount.toString
    }

    def contains(node: Node): Boolean = nodes.contains(node)

    def insert(from: Node, to: Node): Edge = {
      assert(contains(from) || contains(to))
      assert(!(to eq start) && !(from eq end))
      val e = Edge(from, to)
      from.out += e
      to.in += e
      nodes += from
      nodes += to
      e
    }

    def prepend(e: Edge, n: Node): Edge = {
      remove(e)
      insert(e.from, n)
      insert(n, e.to)
    }

    def remove(node: Node): Unit = {
      assert(contains(node) && !((node eq start) || (node eq end)))
      // Disconnect all the in-nodes from this node
      for (in <- node.in) {
        in.from.out -= in
      }
      // Disconnect all of the out-nodes from this node
      for (out <- node.out) {
        out.to.in -= out
      }
      // remove it from the nodes list
      nodes -= node
    }

    def remove(edge: Edge): Unit = {
      edge.to.in -= edge
      edge.from.out -= edge
    }

    override def toString: String = {
      "digraph g {" +
        nodes
          .map(_.out.map("\n" + _.toString + ";").mkString(""))
          .mkString("") +
        nodes.map(node => s"""\n${node.id} [${node.dotLabel}]""").mkString +
        "\n}"
    }
  }

  object Analysis {
    def analyze(node: EirFunction): Graph = {
      val g = new Graph()
      visit(node, List(g.start))(g).map(g.insert(_, g.end))
      g
    }

    private[this] def nameFor(node: EirNode)(implicit graph: Graph): String = {
      graph.nextNode
    }

    private[this] def iterate(nodes: Iterable[EirNode], prev: List[Node])(
        implicit graph: Graph
    ): List[Node] = {
      nodes.foldLeft(prev)((next, y) => {
        visit(y, next)
      })
    }

    private[this] def chain(node: EirNode, prev: List[Node])(implicit
        graph: Graph
    ): List[Node] = {
      if (prev.isEmpty) {
        Nil
      } else {
        val curr = prev.headOption
          .filter(x => prev.length == 1 && !x.locked)
          .getOrElse({
            val n = Node(nameFor(node))
            prev.foreach(graph.insert(_, n))
            n
          })
        curr.statements :+= node
        List(curr)
      }
    }

    private[this] def lockAll(nodes: List[Node]): List[Node] = {
      nodes.foreach(_.locked = true)
      nodes
    }

    private[this] def labelEdges(
        from: List[Node],
        to: List[Node],
        label: String
    ): Unit = {
      for {
        f <- from
        t <- to
      } f.out.filter(_.to == t).foreach(_.label = Some(label))
    }

    private[this] def visit(node: EirNode, prev: List[Node])(implicit
        graph: Graph
    ): List[Node] = {
      if (prev.isEmpty) {
        return Nil
      }

      node match {
        case x: EirFunction => x.body.map(visit(_, prev)).getOrElse(prev)
        case x: EirBlock =>
          val next = Node(nameFor(x))
          prev.foreach(graph.insert(_, next))
          iterate(x.children, List(next))
        case x: EirIfElse =>
          val test = lockAll(visit(x.test, prev))
          val ifTrue = iterate(x.ifTrue, test)
          val ifFalse = iterate(x.ifFalse, test)
          labelEdges(test, ifTrue, "YES")
          labelEdges(test, ifFalse, "NO")
          lockAll(ifTrue ++ ifFalse)
        case x: EirForLoop => x.header match {
            case EirCStyleHeader(decl, test, incr) =>
              val declNodes = iterate(decl, prev)
              val testNodes = iterate(test, lockAll(declNodes))
              val bodyNodes = iterate(x.body, lockAll(testNodes))
              val incrNodes = iterate(incr, lockAll(bodyNodes))

              for {
                testNode <- testNodes
                incrNode <- incrNodes
              } graph.insert(incrNode, testNode)

              labelEdges(testNodes, bodyNodes, "YES")

              testNodes
          }
        case x: EirReturn =>
          lockAll(chain(x, prev)).foreach(graph.insert(_, graph.end))
          Nil
        case x => chain(x, prev)
      }
    }
  }

  class Pass extends passes.Pass {
    override def phase: Phase = Phase.Load

    override def after: Seq[Pass] = Seq()

    override def apply(n: EirNode): Unit = {
      val isMain = n.hasAnnotation("main")
      if (isMain) {
        val asClass = assertValid[EirClass](n)
        asClass.members
          .map(_.member)
          .collect { case f: EirFunction => f }
          .map(Analysis.analyze)
          .foreach(println(_))
      }
    }
  }
}
