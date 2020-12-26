package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirNode, EirSpecializable, EirSpecialization, EirTemplateArgument}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.collection.mutable


class TypeCheckContext {
  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private var _substitutions: List[(EirSpecializable, EirSpecialization)] = List()
  private var _checked: Map[EirSpecializable, List[EirSpecialization]] = Map()

  val goal: mutable.Stack[EirType] = new mutable.Stack

  def checked: Map[EirSpecializable, List[EirSpecialization]] = _checked

  def enterNode(n: EirNode): Unit = {
    stack.push(n)
  }

  def shouldCheck(s: EirSpecializable): Boolean = {
    if (s.annotation("system").isDefined) {
      false
    } else if (s.templateArgs.isEmpty) {
      if (_checked.contains(s)) false
      else { _checked += (s -> Nil); true }
    } else {
      _substitutions.find(_._1 == s).map(_._2) match {
        case Some(sp) => {
          val checked = _checked.getOrElse(s, Nil)
          if (checked.contains(sp)) false
          else {
            _checked += (s -> (checked :+ sp))
            true
          }
        }
        case None => false
      }
    }
  }

  def specialize(s : EirSpecializable): EirSpecialization = {
    val sp = specialization
      .find(_.specialization.length == s.templateArgs.length)
      .getOrElse(Errors.missingSpecialization(s))
    specialize(s, sp)
  }

  def specialize(s : EirSpecializable, sp : EirSpecialization): EirSpecialization = {
    val ours = s.templateArgs.map(Find.uniqueResolution[EirType])
    val theirs = sp.specialization.map(Find.uniqueResolution[EirType])
    if (ours.length != theirs.length) {
      Errors.missingSpecialization(s)
    } else if (ours.zip(theirs).forall(t => t._1 == t._2)) {
      null
    } else {
      _substitutions +:= (s -> sp)
      sp
    }
  }

  def leave(ours: EirSpecialization): Unit = {
    if (ours != null) {
      val first = _substitutions.indexWhere(_._1 == ours)
      _substitutions = _substitutions.patch(first, Nil, 1)
    }
  }

  def specialization: Option[EirSpecialization] = {
    stack.collectFirst {
      case x: EirSpecialization if !_substitutions.exists(y => {
        x.asInstanceOf[AnyRef] eq y._2
      }) && x.specialization.nonEmpty => x
    }
  }


  def hasSubstitution(s: EirSpecializable): Boolean = _substitutions.contains(s)

  def hasSubstitution(t: EirTemplateArgument): Option[EirResolvable[EirType]] = {
    _substitutions.reverse.flatMap({
      case (sable, stion) => sable.templateArgs.zip(stion.specialization)
    }).collectFirst({
      case (arg, ty) if arg == t => ty
    })
  }

  def leaveWith(t: EirType): EirType = {
    stack.pop()
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
