package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{
  EirEncloseExempt,
  EirExpressionPattern,
  EirExtractorPattern,
  EirIdentifierPattern,
  EirImport,
  EirMatchCase,
  EirMember,
  EirNode,
  EirPatternList,
  EirSdagWhen
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

      def enclosable: Iterable[EirNode] = {
        node match {
          case EirPatternList(_, list)           => list
          case EirExpressionPattern(_, expr)     => List(expr)
          case EirExtractorPattern(_, id, list)  => List(id, list)
          case EirMatchCase(_, list, cond, body) => List(list) ++ cond ++ body
          case EirMember(_, member, _)           => List(member)
          case when: EirSdagWhen                 => when.children ++ when.patterns.map(_._2)
          case _                                 => node.children
        }
      }
    }
  }

  private def visit(node: EirNode): Iterable[EirNode] = {
    node.enclosable.flatMap { child =>
      Option.unless(child.validEnclose(node))(child) ++ visit(child)
    }
  }

  def apply(node: EirNode): Option[EirNode] = visit(node).headOption

  def enclose(node: EirNode, scope: Option[EirNode]): Unit = {
    val recurse = () => node.enclosable.foreach(enclose(_, Some(node)))
    val setParent = () => {
      node.parent.nonEmpty || {
        node.parent = scope
        scope.nonEmpty
      }
    }

    node match {
      case _: EirEncloseExempt =>
      case _: EirImport        => setParent()
      case _                   => if (setParent()) recurse()
    }
  }
}
