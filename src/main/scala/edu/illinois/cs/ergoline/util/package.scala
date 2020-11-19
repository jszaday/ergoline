package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.ListHasAsScala

package object util {

  import EirUtilitySyntax.{RichEirNode, RichIntOption}

  def assertValid[T: Manifest](value: Any): T = {
    Option(value) match {
      case Some(x: T) => x
      case x => throw new RuntimeException(s"unexpected value $x")
    }
  }

  def placeNodes(scope: EirScope, nodes: Iterable[EirNode]): Unit = {
    for (node <- nodes) {
      (scope, node) match {
        case (EirGlobalNamespace, x : EirNamespace) =>
          EirGlobalNamespace.put(x.name, x)
        case (x: EirNamespace, _) =>
          x.children +:= node
        case _ => throw new RuntimeException(s"cannot place $node into $scope")
      }
    }
  }

  def dropNodes(scope: EirScope, nodes: Iterable[EirNode]): Unit = {
    for (node <- nodes) {
      scope match {
        case x : EirNamespace => x.removeChild(node)
        case _ => throw new RuntimeException(s"could not drop $node from $scope")
      }
    }
  }

  def encloseNodes(nodes: EirNode*): EirBlock = {
    val b = EirBlock(nodes.head.parent, nodes.toList)
    nodes.foreach(_.parent = Some(b))
    b
  }

  def makeTupleElementGetter(expression: EirExpressionNode, idx: Int): EirExpressionNode = {
    val ref = EirArrayReference(expression.parent, expression, null)
    expression.parent = Some(ref)
    ref.args = List(EirLiteral(Some(ref), EirLiteralTypes.Integer, idx.toString))
    ref
  }

  def visitAll[T](node: EirNode, f: EirNode => T): Seq[T] = {
    f(node) +: node.children.flatMap(n => {
      f(n) +: visitAll(n, f)
    }).toSeq
  }

  def applyOrFalse[T <: EirNode : Manifest](function: T => Unit, value: EirNode): Boolean = {
    value.isValid[T].map(function).isDefined
  }

  def updateWithin[T <: EirNode : Manifest](lst : List[T], oldNode : EirNode, newNode : EirNode): Option[List[T]] = {
    val converted = (oldNode.isValid[T] ++ newNode.isValid[T]).toList
    converted match {
      case List(o, n) =>
        lst.indexOf(o) match {
          case -1 => None
          case idx => Some(lst.updated(idx, n))
        }
      case _ => None
    }
  }

  private def xCanAccessYViaZ(x: EirNode, y: EirNode, z: EirNode): Boolean = {
    // TODO implement this
    true
  }

  private def xImportsY(x : EirNode, y : EirNode): Boolean = {
    true
  }

  private def xCanAccessY(x: EirNode, y: EirNode): Boolean = {
    if (y.isInstanceOf[EirEncloseExempt]) {
      return xImportsY(x, y)
    }
    Find.commonAncestor(x, y).exists {
      case z: EirBlock => (z.findPositionOf(x) > z.findPositionOf(y)) && xCanAccessYViaZ(x, y, z)
      case z => xCanAccessYViaZ(x, y, z)
    }
  }

  // TODO implement this
  def deepCloneTree[T <: EirNode](node: T): T = node

  object EirUtilitySyntax {

    implicit class RichEirNode(node: EirNode) {
      def canAccess(other: EirNode): Boolean = xCanAccessY(node, other)

      def visitAll[T](f: EirNode => T): Seq[T] = util.visitAll(node, f)

      def findChild[T <: EirNode : Manifest](predicate: T => Boolean): Iterable[T] =
        Find.child[T](node, predicate)

      def findWithin[T <: EirNode : Manifest](predicate: T => Boolean): Iterable[T] =
        Find.within[T](node, predicate)

      def isValid[T : Manifest]: Option[T] = node match {
        case t : T => Some(t)
        case _ => None
      }

      def hasName(name : String): Boolean = {
        node match {
          case x : EirNamedNode => x.name == name
          case _ => false
        }
      }
    }

    implicit class RichOption(option: Option[_]) {
      def to[T: Manifest]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }

    implicit class RichIntOption(option: Option[Int]) {
      def >(other: Option[Int]): Boolean = option ++ other match {
        case List(x, y) => x > y
        case _ => false
      }
    }

    implicit class RichResolvableTypeIterable(types: Iterable[EirResolvable[EirType]]) {
      def toTupleType(implicit parent: Option[EirNode]): EirResolvable[EirType] =
        types.toList match {
          case Nil => throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(parent, x)
        }
    }

    implicit class RichParserRuleContext[T <: ParserRuleContext](t: T) {
      def mapOrEmpty[A, B](f: T => java.util.List[A], g: A => B): List[B] =
        Option(t).map(f).map(_.asScala).getOrElse(Nil).map(g).toList
    }

    implicit class RichBoolean(boolean: Boolean) {
      def ifTrue[T](t: Iterable[T]): Iterable[T] = if (boolean) t else None

      def ifFalse[T](t: Iterable[T]): Iterable[T] = if (boolean) None else t

      def enclose[T](t: T): Option[T] = if (boolean) Option(t) else None
    }

  }

}
