package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirNode, EirSpecializable, EirSpecialization, EirTemplateArgument}
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.CheckTypes.error
import edu.illinois.cs.ergoline.resolution.EirResolvable

import scala.collection.mutable


class TypeCheckContext {
  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private val _substitutions: mutable.Stack[(EirSpecializable, EirSpecialization)] = new mutable.Stack

  def enterNode(n: EirNode): Unit = {
    stack.push(n)
    n match {
      case s: EirSpecializable =>
        val ourLength = s.templateArgs.length
        val theirLength = specialization.map(_.specialization).getOrElse(Nil).length
        if (theirLength != ourLength) {
          error(this, n, s"expected $ourLength template arguments, got $theirLength instead")
        }
        else if (ourLength > 0) emplaceSubstitution(s)
      case _ =>
    }
  }

  def emplaceSubstitution(s: EirSpecializable): Unit = {
    _substitutions.push((s, specialization.get))
  }

  def specialization: Option[EirSpecialization] = {
    stack.collectFirst {
      case x: EirSpecialization if !_substitutions.exists(y => {
        x.asInstanceOf[AnyRef] eq y._2
      }) && x.specialization.nonEmpty => x
    }
  }

  def hasSubstitution(t: EirTemplateArgument): Option[EirResolvable[EirType]] = {
    _substitutions.flatMap({
      case (sable, stion) => sable.templateArgs.zip(stion.specialization)
    }).collectFirst({
      case (arg, ty) if arg == t => ty
    })
  }

  def leaveWith(t: EirType): EirType = {
    val leaving = stack.pop()
    if (_substitutions.headOption.exists({
      case (node, _) => leaving == node
    })) {
      _substitutions.pop()
    }
    t
  }

  def alreadyLeft(n: EirNode): Boolean = !stack.headOption.contains(n)

  def cameVia[T: Manifest]: Option[T] = {
    Option.when(stack.length >= 2)(stack(1) match {
      case x: T => Some(x)
      case _ => None
    }).flatten
  }
}
