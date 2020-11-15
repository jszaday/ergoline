package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.resolution.EirResolvable

package object types {

  trait EirType extends EirResolvable[EirType]

  case class EirTupleType(var parent: Option[EirNode], var children: List[EirResolvable[EirType]]) extends EirType {
    override def resolve(scope: EirScope): EirType = EirTupleType(parent, children.map(_.resolve(scope)))
  }

  case class EirLambdaType(var parent: Option[EirNode], var from: List[EirResolvable[EirType]], var to: EirResolvable[EirType]) extends EirType {
    override def resolve(scope: EirScope): EirType = EirLambdaType(parent, from.map(_.resolve(scope)), to.resolve(scope))

    override def children: List[EirResolvable[EirType]] = from ++ List(to)
  }

  case class EirTemplatedType(var parent: Option[EirNode], var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]]) extends EirType {
    override def resolve(scope: EirScope): EirType = EirTemplatedType(parent, base.resolve(scope), args.map(_.resolve(scope)))

    override def children: List[EirResolvable[EirType]] = List(base) ++ args
  }

  case class EirProxyType(var parent: Option[EirNode], var base: EirResolvable[EirType], var collective: Option[String]) extends EirType {
    override def resolve(scope: EirScope): EirType = EirProxyType(parent, base.resolve(scope), collective)

    override def children: List[EirResolvable[EirType]] = List(base)
  }

}
