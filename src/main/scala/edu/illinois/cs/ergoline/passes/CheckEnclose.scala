package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirEncloseExempt, EirMember, EirNode}
import edu.illinois.cs.ergoline.passes.CheckEnclose.CheckEncloseSyntax.RichEirNode
import edu.illinois.cs.ergoline.resolution.EirResolvable

object CheckEnclose {
  object CheckEncloseSyntax {
    implicit class RichEirNode(node : EirNode) {
      def validEnclose(other : EirNode): Boolean = {
        node match {
          /* TODO this needs to be thought about more so it does
           *      not exempt symbols from scrutiny
           */
          case _: EirEncloseExempt | _: EirResolvable[_] => true
          case _ => node.parent.contains(other)
        }
      }
    }
  }

  def visit(node: EirNode): Option[EirNode] = {
    (node match {
      case x: EirMember => List(x.member)
      case _ => node.children
    }).find(!_.validEnclose(node)) match {
      case None => node.children.map(visit).find(_.isDefined).flatten
      case x => x
    }
  }

  def apply(node: EirNode): Option[EirNode] = visit(node)
}
