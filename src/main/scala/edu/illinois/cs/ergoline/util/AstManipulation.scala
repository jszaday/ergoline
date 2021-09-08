package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode

import scala.reflect.ClassTag

object AstManipulation {
  def insertBefore(where: EirNode, node: EirNode): Unit = {
    val block = Find.ancestors(where) collectFirst { case b: EirBlock => b }
    block.zip(block.flatMap(_.findPositionOf(where))) foreach { case (b, pos) =>
      b.insertAt(pos, node)
    }
  }

  def setParent[T <: EirNode](parent: T): T = {
    parent.children.foreach(_.parent = Some(parent))
    parent
  }

  def encloseNodes(
      nodes: EirNode*
  )(implicit addReturn: Boolean = false): EirBlock = {
    AstManipulation.setParent(
      EirBlock(
        nodes.head.parent, {
          if (!addReturn || nodes.length != 1) nodes.toList
          else {
            val ret =
              EirReturn(None, assertValid[EirExpressionNode](nodes.head))
            nodes.head.parent = Some(ret)
            List(ret)
          }
        }
      )
    )
  }

  def dropNodes(scope: EirScope, nodes: Iterable[EirNode]): Unit = {
    for (node <- nodes) {
      scope match {
        case x: EirNamespace => x.removeChild(node)
        case _ =>
          throw new RuntimeException(s"could not drop $node from $scope")
      }
    }
  }

  def placeNode(scope: EirScope, node: EirNode): Unit = {
    (scope, node) match {
      case (EirGlobalNamespace, x: EirNamespace) =>
        EirGlobalNamespace.put(x.name, x)
      case (x: EirNamespace, _) => x.children +:= node
      case _                    => throw new RuntimeException(s"cannot place $node into $scope")
    }
  }

  def updateWithin[T <: EirNode: ClassTag](
      lst: List[T],
      oldNode: EirNode,
      newNode: EirNode
  ): Option[List[T]] = {
    val converted = (oldNode.isValid[T] ++ newNode.isValid[T]).toList
    converted match {
      case List(o, n) => lst.indexOf(o) match {
          case -1  => None
          case idx => Some(lst.updated(idx, n))
        }
      case _ => None
    }
  }
}
