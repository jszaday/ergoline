package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.resolution.EirResolvable

package object types {

  type EirNamedType = EirType with EirNamedNode

  trait EirType extends EirResolvable[EirType]

  case class EirTupleType(var parent: Option[EirNode], var children: List[EirResolvable[EirType]]) extends EirType {
    override def resolved: EirType = EirTupleType(parent, children.map(_.resolved))
  }

  case class EirLambdaType(var parent: Option[EirNode], var from: List[EirResolvable[EirType]], var to: EirResolvable[EirType]) extends EirType {
    override def resolved: EirType = EirLambdaType(parent, from.map(_.resolved), to.resolved)

    override def children: List[EirResolvable[EirType]] = from ++ List(to)
  }

  case class EirTemplatedType(var parent: Option[EirNode], var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]]) extends EirType {
    override def resolved: EirType = EirTemplatedType(parent, base.resolved, args.map(_.resolved))

    override def children: List[EirResolvable[EirType]] = List(base) ++ args
  }

  case class EirProxyType(var parent: Option[EirNode], var base: EirResolvable[EirType], var collective: Option[String]) extends EirType {
    override def resolved: EirType = EirProxyType(parent, base.resolved, collective)

    override def children: List[EirResolvable[EirType]] = List(base)
  }

}
