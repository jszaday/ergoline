package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.Find.asClassLike
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichResolvableTypeIterable
import edu.illinois.cs.ergoline.util.TypeCompatibility.{RichEirClassLike, RichEirType}
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.collection.mutable
import scala.reflect.ClassTag

object TypeCheckContext {
  type Context = (Option[EirClassLike], Option[EirSpecialization])
}

class TypeCheckContext {
  import TypeCheckContext.Context

  object TypeCheckSyntax {
    implicit class RichEirTemplateArgument(argument: EirTemplateArgument) {
      // TODO implement this, check upper/lower/type bounds
      def accepts(resolvable: EirResolvable[EirType])(implicit ctx: TypeCheckContext): Boolean = {
        val ub = argument.upperBound.map(CheckTypes.visit)
        val lb = argument.lowerBound.map(CheckTypes.visit)
        val ty = argument.argumentType.map(CheckTypes.visit)
        val theirs = CheckTypes.visit(resolvable) match {
          case t: EirTupleType if argument.isPack => t.children.map(_.asInstanceOf[EirType])
          case t => List(t)
        }
        theirs.forall(b => {
          (
            ty.zip(Some(b)) forall {
              case (a, b: EirConstantFacade) =>
                CheckTypes.visit(b.value).canAssignTo(a)
              case _ => false
            }
          ) && (
            // Upper Bounds:
            // a must be a subclass of b
            ub.zip(Some(b)) map {
              case (a, b) => (Find.asClassLike(a), Find.asClassLike(b))
            } forall {
              case (a, b) => a.isDescendantOf(b)
              case _ => false
            }
          ) && (
            // Lower Bounds:
            // b must be a subclass of a
            lb.zip(Some(b)) map {
              case (a, b) => (Find.asClassLike(a), Find.asClassLike(b))
            } forall {
              case (a, b) => b.isDescendantOf(a)
              case _ => false
            }
          )
        })
      }
    }

    implicit class RichEirSpecializable(specializable: EirSpecializable) {
      def accepts(specialization: EirSpecialization)(implicit ctx: TypeCheckContext): List[Option[EirResolvable[EirType]]] = {
        val ours = specializable.templateArgs
        val theirs = specialization.types
        val n = math.max(ours.length, theirs.length)

        (0 until n).map({
          case i if i >= theirs.length => ours(i).defaultValue
          case i if i >= ours.length => ours.lastOption.filter(t => t.isPack && t.accepts(theirs(i))).map(_ => theirs(i))
          case i => Option(theirs(i)).filter(ours(i).accepts(_))
        }).toList
      }

      def sameAs(specialization: EirSpecialization): Boolean = {
        val ours = Find.uniqueResolution[EirType](specializable.templateArgs)
        val theirs = Find.uniqueResolution[EirType](specialization.types)
        ours.zip(theirs).forall(t => t._1 == t._2)
      }
    }
  }

  import TypeCheckSyntax.RichEirSpecializable

  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private val _contexts: mutable.Stack[Context] = new mutable.Stack
  private var _substitutions: List[(EirSpecializable, EirSpecialization)] = List()
  private var _checked: Map[EirSpecializable, List[Context]] = Map()
  private var _cache: Map[(Context, EirNode), EirType] = Map()

  def currentNode: Option[EirNode] = stack.headOption

  val goal: mutable.Stack[EirType] = new mutable.Stack

  var lambdas: List[EirLambdaExpression] = Nil

  def numSubst: Int = _substitutions.size
  def removeSubstUntil(n: Int): Unit = {
    if (_substitutions.size > n) {
      _substitutions = _substitutions.patch(0, Nil, _substitutions.size - n)
    }
  }

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
      Find.uniqueResolution[EirSpecializable](t.base),
      t.args.map(CheckTypes.visit(_)(this)))
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

  def mkCheckContext(s: EirSpecializable, spec: Option[EirSpecialization]): Option[Context] = {
    val sp = spec.map(makeDistinct)
    val checked = _checked.getOrElse(s, Nil)
    val ctx = (immediateAncestor[EirMember].map(_.base), sp)
    Option.unless(checked.contains(ctx))({
      _checked += (s -> (checked :+ ctx))
      ctx
    })
  }

  def trySpecialize(s : EirSpecializable): Option[EirSpecialization] = {
    specialization.flatMap(trySpecialize(s, _))
  }

  def synthesize(types: List[EirResolvable[EirType]]): EirSpecialization = {
    EirSyntheticSpecialization(types)
  }

  def trySpecialize(s : EirSpecializable, spec : EirSpecialization): Option[EirSpecialization] = {
    val types = s.accepts(spec)(this)
    Option.when(types.forall(_.isDefined))({
      val sp = Option.when(types.length == spec.types.length)(spec)
                     .getOrElse({ synthesize(types.map(_.get)) })
      Option.unless(s.sameAs(sp))({
        // TODO this needs to substitute template arguments with
        //      whatever is currently in the context~!
        _substitutions +:= (s -> sp)
        sp
      }).orNull
    })
  }

  def specialize(s: EirSpecializable, sp: EirSpecialization): EirSpecialization = {
    trySpecialize(s, sp).getOrElse(Errors.missingSpecialization(sp))
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
      }) && x.types.nonEmpty => x
    }
  }

  def templateZipArgs(s: EirSpecializable, sp: EirSpecialization): List[(EirTemplateArgument, EirResolvable[EirType])] = {
    if (sp.types.length < s.templateArgs.length) {
      s.templateArgs.zip(sp.types ++ {
        s.templateArgs.slice(sp.types.length, s.templateArgs.length).map(x => {
          x.defaultValue.getOrElse(Errors.missingType(x))
        })
      })
    } else {
      val (init, last) = (s.templateArgs.init, s.templateArgs.last)
      if (last.isPack) {
        init.zip(sp.types) :+ {
          (last, sp.types.slice(init.length, sp.types.length).toTupleType(allowUnit = true)(None))
        }
      } else {
        (init :+ last).zip(sp.types)
      }
    }
  }

  def findSubstitution(s: EirSpecializable): Option[EirSpecialization] = {
    _substitutions
      .find(x => (x._1, s) match {
        // NOTE EirLambdaExpression does not have template arguments so it's not considered here
        // case (a: EirProxy, b: EirClass) => a.base == b
        // case (a: EirProxy, b: EirTrait) => a.base == b
        case (a: EirLambdaType, b: EirFunction) => a.templateArgs == b.templateArgs
        case (a, b) => a == b
      }).map(_._2)
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

  def immediateAncestor[T: ClassTag]: Option[T] = {
    Option.when(stack.length >= 2)(stack(1) match {
      case x: T => Some(x)
      case _ => None
    }).flatten
  }

  def ancestor[T: ClassTag]: Option[T] = stack.collectFirst{ case t: T => t }

  def popUntil(node: EirNode): Unit = stack.popWhile(node != _)

  type ResolvableType = EirResolvable[EirType]

  private val lambdaBank: mutable.Map[(List[ResolvableType], ResolvableType, List[EirTemplateArgument]), EirLambdaType] = mutable.Map()

  def lambdaWith(from: List[ResolvableType], to: ResolvableType, args: List[EirTemplateArgument] = Nil): EirLambdaType = {
    val triple = (from, to, args)

    if (!lambdaBank.contains(triple)) {
      lambdaBank.put(triple, EirLambdaType(None, from, to, args))
    }

    lambdaBank(triple)
  }
}
