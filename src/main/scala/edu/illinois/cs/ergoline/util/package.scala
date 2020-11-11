package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.types._

package object util {

  import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

  def find[T: Manifest](fqn: List[String], scope: Option[EirScope]): Option[T] = {
    scope match {
      case Some(s : EirScope) => find(fqn, s)
      case _ => None
    }
  }

  def find[T: Manifest](fqn: List[String], scope: EirScope = EirGlobalNamespace): Option[T] = {
    fqn match {
      case head :: Nil => scope(head).to[T]
      case init :+ last => find[EirScope](init, scope).flatMap(x => x(last)).to[T]
      case _ => None
    }
  }

  def findAll[T: Manifest](scope : EirScope): Iterable[T] = {
    scope.children.collect{
      case x : EirScope => findAll(x)
    }.flatten ++ scope.children.collect {
      case x : T => x
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

  def putIntoScope(node : EirNamespace, scope : EirScope) = {
    scope match {
      case EirGlobalNamespace =>
        EirGlobalNamespace.put(node.name, node)
      case x: EirNamespace =>
        x.children ++= List(node)
      case _ => throw new RuntimeException("unacceptable module target")
    }
    node
  }

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

    override def toString: String = s"Pending(${fqn.mkString("::")})"
  }

  object EirUtilitySyntax {

    implicit class RichStringIterable(fqn: Iterable[String]) {
      def asResolvable[T: Manifest]: EirResolvable[T] = new EirResolvableName[T](fqn)
    }

    implicit class RichAllowed(allowed: Allowed) {
//      def resolve()(implicit scope : EirScope): EirType = (allowed match {
//        case Left(x) => x
//        case Right(x) => x.resolveDefinitely(scope)
//      })

      def name : String = allowed match {
        case Left(x) => x.name
        case Right(x) => x.toString
      }

      def canAssignTo(other: Allowed): Boolean = {
        // TODO implement true comparison between *fully resolved* type objects
        allowed.name == other.name
      }
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

    implicit class RichOption(option : Option[_]) {
      def to[T : Manifest]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }
  }

}
