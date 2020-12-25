package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.ListHasAsScala

package object util {

  import EirUtilitySyntax.RichEirNode

  def makeMemberFunction(parent: EirClassLike, name: String,
                         args: List[EirResolvable[EirType]],
                         retTy: EirResolvable[EirType], isConst: Boolean): EirMember = {
    val m = EirMember(Some(parent), null, EirAccessibility.Public)
    m.annotations +:= EirAnnotation("system", Map())
    m.member = EirFunction(Some(m), None, name, Nil, Nil, retTy)
    m.member.asInstanceOf[EirFunction].functionArgs = args.zipWithIndex.map({
      case (value, i) => EirFunctionArgument(Some(m.member), s"x$i", value, isFinal = false, isSelfAssigning = false)
    })
    m
  }

  def assertValid[T: Manifest](value: Any): T = {
    Option(value) match {
      case Some(x: T) => x
      case x => throw new RuntimeException(s"unexpected value $x")
    }
  }

  def visitAll[T](node: EirNode, f: EirNode => T): Seq[T] = {
    f(node) +: node.children.flatMap(n => {
      f(n) +: visitAll(n, f)
    }).toSeq
  }

  def applyOrFalse[T <: EirNode : Manifest](function: T => Unit, value: EirNode): Boolean = {
    value.isValid[T].map(function).isDefined
  }

  private def xCanAccessY(x: EirNode, y: EirNode): Boolean = {
    y match {
      case _ : EirTemplateArgument => Find.commonAncestor(x, y) == y.parent
      case m : EirMember if m.accessibility == EirAccessibility.Public => true
      case m : EirMember =>
        (Find.parentOf[EirClassLike](x), y.parent) match {
          case (Some(xParent: EirClassLike), Some(yParent: EirClassLike)) =>
            if (m.accessibility == EirAccessibility.Private) xParent == yParent
            else xParent.isDescendantOf(yParent)
          case _ => false
        }
      case _ => true
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

    implicit class RichBoolean(boolean: Boolean) {
      def ifTrue[T](t: Iterable[T]): Iterable[T] = if (boolean) t else None

      def ifFalse[T](t: Iterable[T]): Iterable[T] = if (boolean) None else t

      def enclose[T](t: T): Option[T] = if (boolean) Option(t) else None
    }

  }

}
