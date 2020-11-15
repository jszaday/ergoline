package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.{EirNode, EirScope}
import edu.illinois.cs.ergoline.resolution.EirResolvable

package object types {

  trait EirType extends EirResolvable[EirType] {
    def name: String

    def needsResolution: Boolean = typeChildren.exists {
      case x: EirType => x.needsResolution
      case _ => true
    }

    def typeChildren: List[EirResolvable[EirType]]

    override def represents: Option[EirNode] = ???

    override def toString: String = name
  }

  case class EirTupleType(var elements: List[EirResolvable[EirType]]) extends EirType {
    def name: String = s"(${elements mkString ","})"

    override def resolve(scope: EirScope): EirType = EirTupleType(elements.map(_.resolve(scope)))

    override def typeChildren: List[EirResolvable[EirType]] = elements
  }

  case class EirLambdaType(var from: List[EirResolvable[EirType]], var to: EirResolvable[EirType]) extends EirType {
    def name: String = s"$from => $to"

    override def resolve(scope: EirScope): EirType = EirLambdaType(from.map(_.resolve(scope)), to.resolve(scope))

    override def typeChildren: List[EirResolvable[EirType]] = from ++ List(to)
  }

  case class EirTemplatedType(var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]]) extends EirType {
    override def name: String = s"$base<${args mkString ","}>"

    override def resolve(scope: EirScope): EirType = EirTemplatedType(base.resolve(scope), args.map(_.resolve(scope)))

    override def typeChildren: List[EirResolvable[EirType]] = List(base) ++ args
  }

  case class EirProxyType(var base: EirResolvable[EirType], var collective: Option[String]) extends EirType {
    override def name: String = s"$base@${collective.getOrElse("")}"

    override def resolve(scope: EirScope): EirType = EirProxyType(base.resolve(scope), collective)

    override def typeChildren: List[EirResolvable[EirType]] = List(base)
  }

}
