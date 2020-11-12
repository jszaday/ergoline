package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.types.{EirTupleType, EirType}

package object util {
  def putIntoScope(node: EirNamespace, scope: EirScope): EirNamespace = {
    scope match {
      case EirGlobalNamespace =>
        EirGlobalNamespace.put(node.name, node)
      case x: EirNamespace =>
        x.children ++= List(node)
      case _ => throw new RuntimeException("unacceptable module target")
    }
    node
  }

  object EirUtilitySyntax {

    implicit class RichOption(option: Option[_]) {
      def to[T: Manifest]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }

    implicit class RichResolvableTypeIterable(types: Iterable[EirResolvable[EirType]]) {
      def asType: EirResolvable[EirType] =
        types.toList match {
          case Nil => throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(x)
        }
    }

  }

}
