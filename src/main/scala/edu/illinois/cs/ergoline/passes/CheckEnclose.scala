package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{
  EirEncloseExempt,
  EirImport,
  EirMember,
  EirNode
}
import edu.illinois.cs.ergoline.passes.CheckEnclose.CheckEncloseSyntax.RichEirNode
import edu.illinois.cs.ergoline.resolution.EirResolvable

import scala.annotation.tailrec

object CheckEnclose {
  object CheckEncloseSyntax {
    implicit class RichEirNode(node: EirNode) {
      def validEnclose(other: EirNode): Boolean = {
        node match {
          /* TODO this needs to be thought about more so it does
           *      not exempt symbols from scrutiny
           */
          case _: EirEncloseExempt | _: EirResolvable[_] => true
          case _                                         => node.parent.contains(other)
        }
      }
    }
  }

  private def visit(node: EirNode): Iterable[EirNode] = {
    val children = node match {
      case m: EirMember => Iterable(m.member)
      case _            => node.children.view
    }

    children.filterNot(_.validEnclose(node)) ++
      children.flatMap(visit)
  }

  def apply(node: EirNode): Option[EirNode] = visit(node).headOption

  def enclose(node: EirNode, scope: Option[EirNode]): Unit = {
    val setParent = () => node.parent = node.parent.orElse(scope)
    val recurse = () => node.children.foreach(enclose(_, Some(node)))

    node match {
      case _: EirEncloseExempt =>
      case _: EirImport        => setParent()
      case _ =>
        setParent()
        recurse()
    }
  }
}
