package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.proxies.ProxyManager
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.util
import edu.illinois.cs.ergoline.util.AstManipulation

package object types {

  trait EirType extends EirResolvable[EirType] {
    override def resolve(): List[EirType] = List(this)

    override def resolved: Boolean = true
  }

  type EirNamedType = EirType with EirNamedNode

  case class EirTupleType(var parent: Option[EirNode], var children: List[EirResolvable[EirType]]) extends EirType {
    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      AstManipulation.updateWithin(children, oldNode, newNode).map(children = _).isDefined
    }
  }

  case class EirTupleMultiply(var lhs: EirResolvable[EirType], var rhs: EirExpressionNode)(var parent: Option[EirNode]) extends EirType {
    override def children: Iterable[EirNode] = List(lhs, rhs)
    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
  }

  case class EirLambdaType(var parent: Option[EirNode],
                           var from: List[EirResolvable[EirType]], var to: EirResolvable[EirType],
                           var templateArgs: List[EirTemplateArgument] = Nil) extends EirType with EirSpecializable {
    override def children: List[EirResolvable[EirType]] = from ++ List(to)

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      AstManipulation.updateWithin(from, oldNode, newNode).map(from = _).isDefined ||
        ((to == oldNode) && util.applyOrFalse[EirResolvable[EirType]](to = _, newNode))
    }

    override def equals(any: Any): Boolean = {
      any match {
        case EirLambdaType(_, theirFrom, theirTo, theirArgs) =>
          (from == theirFrom) && (theirTo == to) && (theirArgs == templateArgs)
        case _ => false
      }
    }
  }

  case class EirTemplatedType(var parent: Option[EirNode], var base: EirResolvable[EirType], var args: List[EirResolvable[EirType]])
    extends EirType with EirSpecialization {

    override def children: List[EirResolvable[EirType]] = List(base) ++ args

    // TODO upon resolution TT's should register with their base class
    //      so the compiler knows which specializations to forward-declare!

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      AstManipulation.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
        ((base == oldNode) && util.applyOrFalse[EirResolvable[EirType]](base = _, newNode))
    }

    override def equals(any: Any): Boolean = {
      any match {
        case EirTemplatedType(_, theirBase, theirArgs) =>
          (base == theirBase) && (theirArgs == args)
        case _ => false
      }
    }

    override def types: List[EirResolvable[EirType]] = args
  }

  case class EirProxyType(var parent: Option[EirNode], var base: EirResolvable[EirType], var collective: Option[String], var isElement: Boolean) extends EirType {
    override def children: Iterable[EirResolvable[EirType]] = Seq(base)

    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
      (base == oldNode) && util.applyOrFalse[EirResolvable[EirType]](base = _, newNode)
    }

    private var _resolved: Option[EirType] = None

    override def resolved: Boolean = _resolved.isDefined

    override def resolve(): List[EirType] = {
//      if (_resolved.isEmpty) _resolved = Some(ProxyManager.proxyFor(this))
      _resolved.toList
    }
  }

}
