package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirLambdaExpression, EirMember, EirNode, EirSpecializable, EirSpecialization, EirTemplateArgument}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.{Errors, assertValid}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

import scala.collection.mutable


class TypeCheckContext {
  type Context = (Option[EirClassLike], Option[EirSpecialization])
  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private var _substitutions: List[(EirSpecializable, EirSpecialization)] = List()
  private var _checked: Map[EirSpecializable, List[Context]] = Map()

  val goal: mutable.Stack[EirType] = new mutable.Stack

  var lambdas: List[EirLambdaExpression] = Nil

  // naively filters out partial specializations
  def checked: Map[EirSpecializable, List[EirSpecialization]] = {
    _checked.filterNot(_._1.isInstanceOf[EirProxy]).map{
      case (s, sp) => (s, sp.map(_._2).distinct.collect({
        case Some(s) => s
      }).filterNot(x => {
        // TODO this may need to cross-check template
        //      arguments that do not belong to us?
        x.specialization
          .map(Find.uniqueResolution[EirType])
          .exists(_.isInstanceOf[EirTemplateArgument])
      }))
    }.filter(t => {
      t._1.templateArgs.isEmpty || t._2.nonEmpty
    })
  }

  def enterNode(n: EirNode): Unit = {
    stack.push(n)
  }

  private var _templates: Map[(EirSpecializable, List[EirType]), EirTemplatedType] = Map()

  def getTemplatedType(s: EirSpecializable, args: List[EirType]): EirTemplatedType = {
    if (_templates.contains((s, args))) _templates((s, args))
    else {
      val ty = EirTemplatedType(None, assertValid[EirType](s), args)
      _templates += ((s, args) -> ty)
      ty
    }
  }

  def getTemplatedType(t: EirTemplatedType): EirTemplatedType = {
    getTemplatedType(
      assertValid[EirSpecializable](Find.uniqueResolution[EirType](t.base)),
      t.args.map(CheckTypes.visit(this, _)))
  }

  def makeDistinct(s: EirSpecialization): EirSpecialization = {
    s match {
      case t: EirTemplatedType => getTemplatedType(t)
      case _ => s
    }
  }

  def shouldCheck(s: EirSpecializable, sp: Option[EirSpecialization]): Boolean = {
    val checked = _checked.getOrElse(s, Nil)
    val ctx = (immediateAncestor[EirMember].map(_.base), sp)
    !(checked.contains(ctx) || {
      _checked += (s -> (checked :+ ctx))
      false
    })
  }

  def shouldCheck(s: EirSpecializable): Boolean = {
    if (s.annotation("system").isDefined) {
      false
    } else if (s.templateArgs.isEmpty) {
      shouldCheck(s, None)
    } else {
      _substitutions.find(_._1 == s).map(x => makeDistinct(x._2)) match {
        case Some(sp) => shouldCheck(s, Some(sp))
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
      // TODO this needs to substitute template arguments with
      //      whatever is currently in the context~!
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

  def immediateAncestor[T: Manifest]: Option[T] = {
    Option.when(stack.length >= 2)(stack(1) match {
      case x: T => Some(x)
      case _ => None
    }).flatten
  }

  def ancestor[T: Manifest]: Option[T] = stack.collectFirst{ case t: T => t }

  def popUntil(node: EirNode): Unit = stack.popWhile(node != _)
}
