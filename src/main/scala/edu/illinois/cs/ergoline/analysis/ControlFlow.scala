package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast.{
  EirAwaitMany,
  EirBlock,
  EirCStyleHeader,
  EirClass,
  EirDoWhileLoop,
  EirExpressionNode,
  EirForAllHeader,
  EirForLoop,
  EirFunction,
  EirIfElse,
  EirMatch,
  EirMatchCase,
  EirNode,
  EirReturn,
  EirScope,
  EirSdagWhen,
  EirWhileLoop
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
      var label: Option[String] = None,
      var shape: String = "rectangle"
  ) {
    val in: mutable.Set[Edge] = mutable.Set[Edge]()
    val out: mutable.Set[Edge] = mutable.Set[Edge]()
    var statements: List[EirNode] = Nil
    var locked: Boolean = false
    var style: Option[String] = None

    def dotAttributes: String = {
      (List(
        "label=\"" + label.getOrElse({
          if (this.statements.isEmpty) id
          else {
            statements
              .map(_.toString)
              .map(_.replace("\"", "\\\""))
              .mkString("\\n")
          }
        }) + "\"",
        s"shape=$shape"
      ) ++ style.map(s => s"style=$s")).mkString(", ")
    }

    def reaches(n: Node, without: Node): Boolean = reaches(n, Set(without))

    def reaches(n: Node, without: Set[Node] = Set.empty): Boolean = {
      val seen = mutable.Set[Node]()
      val stack = mutable.Stack[Node](this)

      while (stack.nonEmpty) {
        val m = stack.pop()
        if (m.eq(n)) {
          return true
        } else {
          seen += m
          // Push all unseen children not in without onto the stack
          stack
            .pushAll(
              m.out
                .map(_.to)
                .filter(n => !(seen.contains(n) || without.contains(n)))
            )
        }
      }

      false
    }
  }

  class Graph {
    val start: Node = Node("start")
    val end: Node = Node("end")
    val nodes: mutable.Set[Node] = mutable.Set[Node](start, end)
    private[this] var _nodeCount = 0

    nodes.foreach(_.style = Some("rounded"))

    def nextNode: String = {
      _nodeCount += 1
      _nodeCount.toString
    }

    def contains(node: Node): Boolean = nodes.exists(_.eq(node))

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

    def reachingSuccessors(from: List[Node], to: List[Node]): List[Node] = {
      if ((from.length == 1) && (to.length == 1)) {
        from.head.out.map(_.to).filter(_.reaches(to.head)).toList
      } else {
        ???
      }
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
        nodes
          .map(node => s"""\n${node.id} [${node.dotAttributes}]""")
          .mkString +
        "\n}"
    }
  }

  object Analysis {
    def analyze(fn: EirFunction): Graph = {
      val g = new Graph()
      val init = List(g.start)
      val last = fn.body.map(visit(_, init)(g)).getOrElse(init)
      last.foreach(g.insert(_, g.end))
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
      nodes
        .filterNot(_.statements.isEmpty)
        .foreach(_.locked = true)
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

    private[this] def interconnect(from: List[Node], to: List[Node])(implicit
        graph: Graph
    ): List[Edge] = {
      for {
        f <- from
        t <- to
      } yield graph.insert(f, t)
    }

    private[this] def acquireOrCreate(nodes: List[Node])(
        label: Option[String],
        shape: String
    )(implicit
        graph: Graph
    ): List[Node] = {
      if (
        nodes.headOption.exists(head =>
          !head.locked && head.statements.isEmpty && (nodes.length == 1)
        )
      ) {
        val next = nodes.head
        next.label = label
        next.shape = shape
        List(next)
      } else {
        val next = Node(graph.nextNode, label, shape)
        lockAll(nodes).foreach(graph.insert(_, next))
        List(next)
      }
    }

    private[this] def visit(node: EirNode, prev: List[Node])(implicit
        graph: Graph
    ): List[Node] = {
      if (prev.isEmpty) {
        return Nil
      }

      node match {
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
              interconnect(incrNodes, testNodes)
              labelEdges(testNodes, bodyNodes, "YES")
              testNodes
            case EirForAllHeader(_, _, expr) =>
              val hdrNodes = chain(expr, lockAll(prev))
              val bodyNodes = iterate(x.body, lockAll(hdrNodes))
              interconnect(bodyNodes, hdrNodes)
              hdrNodes
          }
        case x: EirMatch =>
          val hdrNodes = chain(x.expression, lockAll(prev))
          val caseNodes = x.cases.flatMap(visit(_, lockAll(hdrNodes)))
          lockAll(caseNodes)
        case x: EirAwaitMany =>
          if (x.waitAll) {
            val test =
              acquireOrCreate(prev)(Some("completed all paths?"), "diamond")
            test.foreach(_.locked =
              true
            ) // lockAll skips empty nodes, so force locked here
            val cases = x.children.flatMap(visit(_, test))
            interconnect(cases, test)
            test
          } else {
            x.children.flatMap(visit(_, lockAll(prev)))
          }
        case x: EirDoWhileLoop =>
          val bodyNodes = iterate(x.body, lockAll(prev))
          val testNodes = chain(x.condition, bodyNodes)
          val reaching = graph.reachingSuccessors(prev, bodyNodes)
          interconnect(testNodes, reaching).foreach(_.label = Some("YES"))
          lockAll(testNodes)
        case x: EirWhileLoop =>
          val testNodes = chain(x.condition, lockAll(prev))
          val bodyNodes = iterate(x.body, lockAll(testNodes))
          interconnect(bodyNodes, testNodes)
          labelEdges(testNodes, bodyNodes, "YES")
          testNodes
        case x: EirReturn =>
          lockAll(chain(x, prev)).foreach(graph.insert(_, graph.end))
          Nil
        case x: EirMatchCase => iterate(x.body, prev)
        case x: EirSdagWhen =>
          val patterns = x.patterns
            .map(_._1)
            .foldLeft(prev)((next, ptn) => {
              visit(ptn, next)
            })
          iterate(x.body, lockAll(patterns))
        case x => chain(x, prev)
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
        .map(Analysis.analyze)
        .foreach(println(_))
    }
  }
}
