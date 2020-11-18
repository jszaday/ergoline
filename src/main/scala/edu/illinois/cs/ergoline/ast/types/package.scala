package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.util

package object types {

  trait EirType extends EirResolvable[EirType] {
    def resolve(): EirType = this
    def resolved: Boolean = true
    def member(name : String): Option[EirMember] = None
  }

  type EirNamedType = EirType with EirNamedNode

  case class EirBuiltInType(name : String) extends EirType with EirNamedNode with EirEncloseExempt {
    override var parent: Option[EirNode] = Some(EirGlobalNamespace)
    override def children: Iterable[EirNode] = None
    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
  }

  object EirBuiltInTypes {
    val keywords = List("unit", "int", "float", "string")
    val types: Map[String, EirBuiltInType] = keywords.zip(keywords.map(EirBuiltInType)).toMap

    def contains(name : String): Option[EirType with EirNamedNode] = {
      Option.when(types.contains(name))(types(name))
    }
  }

  case class EirTupleType(var parent: Option[EirNode], var children: List[EirResolvable[EirType]]) extends EirType {
    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      util.updateWithin(children, oldNode, newNode).map(children = _).isDefined
    }
  }

  case class EirLambdaType(var parent: Option[EirNode], var from: List[EirResolvable[EirType]], var to: EirResolvable[EirType]) extends EirType {
    override def children: List[EirResolvable[EirType]] = from ++ List(to)

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      util.updateWithin(from, oldNode, newNode).map(from = _).isDefined ||
        ((to == oldNode) && util.applyOrFalse[EirResolvable[EirType]](to = _, newNode))
    }
  }

  case class EirTemplatedType(var parent: Option[EirNode], var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]]) extends EirType {
    override def children: List[EirResolvable[EirType]] = List(base) ++ args

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      util.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
        ((base == oldNode) && util.applyOrFalse[EirResolvable[EirType]](base = _, newNode))
    }
  }

  case class EirProxyType(var parent: Option[EirNode], var base: EirResolvable[EirType], var collective: Option[String]) extends EirType {
    override def children: Iterable[EirResolvable[EirType]] = Seq(base)

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      (base == oldNode) && util.applyOrFalse[EirResolvable[EirType]](base = _, newNode)
    }
  }
}
