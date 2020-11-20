package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.passes.FullyResolve
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.util

package object types {

  trait EirType extends EirResolvable[EirType] {
    override def resolve(): List[EirType] = List(this)
    override def resolved: Boolean = true
  }

  type EirNamedType = EirType with EirNamedNode

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

    override def equals(any: Any): Boolean = {
      any match {
        case EirLambdaType(_, theirFrom, theirTo) =>
          (from == theirFrom) && (theirTo == to)
        case _ => false
      }
    }
  }

  case class EirTemplatedType(var parent: Option[EirNode], var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]]) extends EirType {
    override def children: List[EirResolvable[EirType]] = List(base) ++ args
    private var _registered = false

    override def resolve(): List[EirType] = {
      if (_registered) {
        FullyResolve.visit(this)
        _registered = base.resolve() match {
          case x : EirClassLike => x.putSpecialization(this); true
          case _ => false
        }
      }
      List(this)
    }

    override def resolved: Boolean = _registered

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
