package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.types.{EirTupleType, EirType}
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.ListHasAsScala

package object util {
  def putIntoScope(node: EirNamespace, scope: Option[EirScope]): EirNamespace = {
    scope match {
      case Some(EirGlobalNamespace) =>
        EirGlobalNamespace.put(node.name, node)
      case Some(x: EirNamespace) =>
        x.children ++= List(node)
      case _ => throw new RuntimeException("unacceptable module target")
    }
    node
  }

  import EirUtilitySyntax.RichOption;

  def createOrGetNamespace(fqn: Iterable[String], scope: Option[EirScope]): EirNamespace = {
    fqn.toList match {
      case head :: Nil => scope.flatMap(_.apply(head)).to[EirNamespace].getOrElse(putIntoScope(EirNamespace(scope, Nil, head), scope))
      case init :+ last => createOrGetNamespace(List(last), Some(createOrGetNamespace(init, scope)))
      case _ => null
    }
  }

  object EirUtilitySyntax {

    implicit class RichOption(option: Option[_]) {
      def to[T: Manifest]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }

    implicit class RichResolvableTypeIterable(types: Iterable[EirResolvable[EirType]]) {
      def toTupleType: EirResolvable[EirType] =
        types.toList match {
          case Nil => throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(x)
        }
    }

    implicit class RichParserRuleContext[T <: ParserRuleContext](t: T) {
      def mapOrEmpty[A, B](f: T => java.util.List[A], g: A => B): List[B] =
        Option(t).map(f).map(_.asScala).getOrElse(Nil).map(g).toList
    }
  }

}
