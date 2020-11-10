package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichStringIterable

package object util {

  def find[T: Manifest](fqn: Iterable[String], scope: EirScope = EirGlobalNamespace): Option[T] =
    find[T](fqn.asResolvable[EirNamedNode], scope)

  def find[T: Manifest](resolvable: EirResolvable[EirNamedNode], scope: EirScope): Option[T] = {
    resolvable.resolve(scope) match {
      case Some(EirMember(_, x: T, _)) => Some(x)
      case Some(x: T) => Some(x)
      case _ => None
    }
  }

  def findMatching[T: Manifest](pattern : PartialFunction[EirNode, T], scope : EirScope): Iterable[T] = {
    scope.children.collect{
      case x : EirScope => findMatching(pattern, x)
    }.flatten ++ scope.children.collect(pattern)
  }

  def findAnnotated[T <: EirNode: Manifest](name : String, scope : EirScope): Iterable[T] =
    findMatching[T]({
      case x : T if x.annotations.exists(_.name == name) => x
    }, scope)

  trait EirResolvable[T] {
    def resolve(scope: EirScope): Option[T]

    def resolveDefinitely(scope: EirScope): T = resolve(scope).get
  }

  class EirResolvableName[T: Manifest](var fqn: Iterable[String]) extends EirResolvable[T] {
    def resolve(scope: EirScope): Option[T] = {
      fqn.foldLeft[Option[EirNode]](Some(scope))({
        case (Some(s: EirScope), name) => s(name)
        case _ => None
      }) match {
        case Some(x: T) => Some(x)
        case _ => None
      }
    }

    override def equals(obj: Any): Boolean = obj match {
      case x: EirResolvableName[_] => x.fqn == this.fqn
      case _ => false
    }
  }

  object EirUtilitySyntax {

    implicit class RichStringIterable(fqn: Iterable[String]) {
      def asResolvable[T: Manifest]: EirResolvable[T] = new EirResolvableName[T](fqn)
    }

    implicit class RichAllowedIterable(types: Iterable[Allowed]) {
      def asType: Allowed =
        types.toList match {
          case Nil => throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(x).asAllowed
        }
    }

    implicit class RichResolvable(r: EirResolvable[EirType])(implicit scope: EirScope) {
      def asType: EirResolvableType = EirResolvableType(scope, r)

      def asAllowed: Allowed = Right(r)
    }

    implicit class RichType(t: EirType) {
      def asAllowed: Allowed = Left(t)
    }

  }

}
