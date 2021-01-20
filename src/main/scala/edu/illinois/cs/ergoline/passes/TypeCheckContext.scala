package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichResolvableTypeIterable
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.collection.mutable

class TypeCheckContext {
  object TypeCheckSyntax {
    implicit class RichEirTemplateArgument(argument: EirTemplateArgument) {
      // TODO implement this, check upper/lower/type bounds
      def accepts(resolvable: EirResolvable[EirType]): Boolean = true
    }

    implicit class RichEirSpecializable(specializable: EirSpecializable) {
      def accepts(specialization: EirSpecialization): Boolean = {
        val ours = specializable.templateArgs
        val theirs = specialization.specialization
        val n = math.max(ours.length, theirs.length)
        var i = 0
        while (i < n) {
          if (i >= theirs.length) {
            if (!ours(i).hasDefaultValue) {
              return false
            }
          } else if (i >= ours.length) {
            if (!ours.lastOption.exists(t => t.isPack && t.accepts(theirs(i)))) {
              return false
            }
          } else if (!ours(i).accepts(theirs(i))) {
            return false
          }
          i += 1
        }
        true
      }

      def sameAs(specialization: EirSpecialization): Boolean = {
        val ours = Find.uniqueResolution[EirType](specializable.templateArgs)
        val theirs = Find.uniqueResolution[EirType](specialization.specialization)
        ours.zip(theirs).forall(t => t._1 == t._2)
      }
    }
  }

  import TypeCheckSyntax.RichEirSpecializable

  type Context = (Option[EirClassLike], Option[EirSpecialization])
  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private val _contexts: mutable.Stack[Context] = new mutable.Stack
  private var _substitutions: List[(EirSpecializable, EirSpecialization)] = List()
  private var _checked: Map[EirSpecializable, List[Context]] = Map()
  private var _cache: Map[(Context, EirNode), EirType] = Map()

  val goal: mutable.Stack[EirType] = new mutable.Stack

  var lambdas: List[EirLambdaExpression] = Nil

  // naively filters out partial specializations
  def checked: Map[EirSpecializable, List[EirSpecialization]] = {
    _checked.map{
      case (sp, contexts) => (sp,
        contexts.collect({ case (_, Some(sp)) => sp }).distinct)
    }
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

  private var _tuples: Map[List[EirType], EirTupleType] = Map()

  def getTupleType(_ts: Iterable[EirType]): EirTupleType = {
    val ts = _ts.toList
    _tuples.getOrElse(ts, {
      val u = EirTupleType(None, ts)
      _tuples += (ts -> u)
      u
    })
  }

  def start(c: Context): Unit = _contexts.push(c)
  def stop(c: Context): Unit = assert(_contexts.pop() == c)
  def current: Option[Context] = _contexts.headOption
  def cache(n: EirNode, t: EirType): Unit = current.foreach(c => _cache += ((c, n) -> t))
  def avail(n: EirNode): Option[EirType] = current.flatMap(c => _cache.get((c, n)))

  def shouldCheck(s: EirSpecializable, sp: Option[EirSpecialization]): Option[Context] = {
    val checked = _checked.getOrElse(s, Nil)
    val ctx = (immediateAncestor[EirMember].map(_.base), sp)
    Option.unless(checked.contains(ctx))({
      _checked += (s -> (checked :+ ctx))
      ctx
    })
  }

  def shouldCheck(s: EirSpecializable): Option[Context] = {
    if (s.annotation("system").isDefined) {
      None
    } else if (s.templateArgs.isEmpty) {
      shouldCheck(s, None)
    } else {
      _substitutions
        .find(x => (x._1, s) match {
          case (a: EirProxy, b: EirClass) => a.base == b
          case (a: EirProxy, b: EirTrait) => a.base == b
          case (a, b) => a == b
        })
        .map(x => makeDistinct(x._2))
        .flatMap(x => shouldCheck(s, Some(x)))
    }
  }

  def specialize(s : EirSpecializable): EirSpecialization = {
    val sp = specialization.find(s.accepts(_)).getOrElse(Errors.missingSpecialization(s))
    specialize(s, sp)
  }

  def specialize(s : EirSpecializable, sp : EirSpecialization): EirSpecialization = {
    if (!s.accepts(sp)) {
      Errors.missingSpecialization(s)
    } else if (s.sameAs(sp)) {
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

  def templateZipArgs(s: EirSpecializable, sp: EirSpecialization): List[(EirTemplateArgument, EirResolvable[EirType])] = {
    if (sp.specialization.length < s.templateArgs.length) {
      s.templateArgs.zip(sp.specialization ++ {
        s.templateArgs.slice(sp.specialization.length, s.templateArgs.length).map(x => {
          x.defaultValue.getOrElse(Errors.missingType(x))
        })
      })
    } else {
      val (init, last) = (s.templateArgs.init, s.templateArgs.last)
      if (last.isPack) {
        init.zip(sp.specialization) :+ {
          (last, sp.specialization.slice(init.length, sp.specialization.length).toTupleType(None))
        }
      } else {
        (init :+ last).zip(sp.specialization)
      }
    }
  }

  def hasSubstitution(t: EirTemplateArgument): Option[EirResolvable[EirType]] = {
    _substitutions.reverse.flatMap({
      case (s, sp) => templateZipArgs(s, sp)
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
