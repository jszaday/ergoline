package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirNode, EirScope}

package object util {
  trait EirResolvable[T] {
    def resolve(scope: EirScope): Option[T]

    def resolveDefinitely(scope: EirScope): T = resolve(scope).get
  }

  class EirResolvableName[T : Manifest](fqn: Iterable[String]) extends EirResolvable[T] {
    def resolve(scope: EirScope): Option[T] = {
      fqn.foldLeft[Option[EirNode]](Some(scope))({
        case (Some(s: EirScope), name) => s(name)
        case _ => None
      }) match {
        case Some(x: T) => Some(x)
        case _ => None
      }
    }
  }

  object EirResolvable {
    def apply[T : Manifest](it: Iterable[String]): EirResolvable[T] = new EirResolvableName[T](it)
  }
}
