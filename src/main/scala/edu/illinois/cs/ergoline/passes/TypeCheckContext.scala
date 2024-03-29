package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{
  EirLambdaType,
  EirTemplatedType,
  EirTupleType,
  EirType
}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.Transactions.EirSubstituteTransaction
import edu.illinois.cs.ergoline.resolution.{
  EirPlaceholder,
  EirResolvable,
  EirTemplateFacade,
  Find,
  Transactions
}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichResolvableTypeIterable
import edu.illinois.cs.ergoline.util.TypeCompatibility.{
  RichEirClassLike,
  RichEirType
}
import edu.illinois.cs.ergoline.util.{Errors, TupleFactory, assertValid}

import scala.collection.mutable
import scala.reflect.ClassTag

object TypeCheckContext {
  type Context = (Option[EirClassLike], Option[EirSpecialization])

  // B >: A refers to B is a supertype of A
  def lowerBound(x: EirType, y: EirType): Boolean = {
    val (clx, cly) = (Find.asClassLike(x), Find.asClassLike(y))
    clx == cly || cly.isDescendantOf(clx)
  }

  // B <: A refers to B is a subtype of A
  def upperBound(x: EirType, y: EirType): Boolean = lowerBound(y, x)

  case class ExpressionScope(
      args: Option[EirExpressionNode],
      acc: Option[EirExpressionNode]
  )

  object TypeCheckSyntax {
    implicit class RichEirTemplateArgument(argument: EirTemplateArgument) {
      // TODO implement this, check upper/lower/type bounds
      def accepts(
          resolvable: EirResolvable[EirType]
      )(implicit ctx: TypeCheckContext): Boolean = {
        resolvable match {
          case _: EirTemplateFacade => return true
          case _                    => ;
        }

        val ub = argument.upperBound.map(CheckTypes.visit)
        val lb = argument.lowerBound.map(CheckTypes.visit)
        val ty = argument.argumentType.map(CheckTypes.visit)
        val theirs = CheckTypes.visit(resolvable) match {
          case t: EirTupleType if argument.isPack =>
            t.children.map(_.asInstanceOf[EirType])
          case t => List(t)
        }
        theirs.forall(a => {
          (
            ty.zip(Some(a)) forall {
              case (b, a: EirConstantFacade) =>
                CheckTypes.visit(a.value).canAssignTo(b)
              case _ => false
            }
          ) && (
            // Upper Bounds:
            // a must be a subclass of b
            ub.forall(b => upperBound(a, b))
          ) && (
            // Lower Bounds:
            // a must be a supertype of b
            lb.forall(b => lowerBound(a, b))
          )
        })
      }
    }

    implicit class RichEirSpecializable(specializable: EirSpecializable) {
      def accepts(
          specialization: EirSpecialization
      )(implicit
          ctx: TypeCheckContext
      ): List[Option[EirResolvable[EirType]]] = {
        val ours = specializable.templateArgs
        val theirs = specialization.types
        val n = math.max(ours.length, theirs.length)

        (0 until n)
          .map({
            case i if i >= theirs.length => ours(i).defaultValue
            case i if i >= ours.length =>
              ours.lastOption
                .filter(t => t.isPack && t.accepts(theirs(i)))
                .map(_ => theirs(i))
            case i => Option(theirs(i)).filter(ours(i).accepts(_))
          })
          .toList
      }

      def sameAs(args: List[EirType]): Boolean = {
        val ours = Find.uniqueResolution[EirType](specializable.templateArgs)
        ours.zip(args).forall(t => t._1 == t._2)
      }
    }
  }
}

class TypeCheckContext(parent: Option[TypeCheckContext] = None)
    extends Transactions.Manager[EirSubstituteTransaction] {
  import TypeCheckContext._
  import TypeCheckSyntax.RichEirSpecializable

  private val stack: mutable.Stack[EirNode] = new mutable.Stack
  private val _contexts: mutable.Stack[Context] = new mutable.Stack
  private def _substitutions = this.transactions
  private var _checked: Map[EirSpecializable, List[Context]] = Map()
  private var _cache: Map[(Context, EirNode), EirType] = Map()

  var staticTyping: Boolean = false

  def currentNode: Option[EirNode] = stack.headOption

  val goal: mutable.Stack[EirType] = new mutable.Stack

  private var _userLambdas: Set[EirLambdaExpression] = Set()
  private var _generatedLambdas: Map[EirExpressionNode, EirLambdaExpression] =
    Map()

  def lambdas: List[EirLambdaExpression] =
    (_userLambdas ++ _generatedLambdas.values).toList

  def registerLambda(x: EirLambdaExpression): Unit = _userLambdas += x

  def makeLambda(
      in: EirExpressionNode,
      m: EirMember,
      ty: EirLambdaType
  ): EirLambdaExpression = {
    var from: List[EirResolvable[EirType]] = ty.from
    val x = in match {
      case EirCallArgument(expr, _) => expr
      case _                        => in
    }

    _generatedLambdas.getOrElse(
      x, {
        val args = assertValid[EirFunction](m.member).functionArgs
        val lambda = EirLambdaExpression(Some(x), args, null)
        _generatedLambdas += (x -> lambda)
        lambda.disambiguation = Some(m)
        lambda.foundType = Some(ty)
        lambda.body = EirBlock(Some(lambda), Nil)
        val ret = EirReturn(Some(lambda.body), null)
        lambda.body.children +:= ret
        val accessor = EirScopedSymbol[EirMember](
          x,
          EirPlaceholder[EirMember](None, Some(m))
        )(None)
        accessor.disambiguation = m
        accessor.isStatic = CheckTypes.isStatic(x)
        ret.expression = EirFunctionCall(
          Some(ret),
          accessor,
          args.map(x =>
            EirCallArgument(
              {
                val p = EirPlaceholder(None, Some(x))
                p.foundType = Some(x.declaredType match {
                  case t: EirPackExpansion if x.isExpansion =>
                    val next = from.toTupleType(allowUnit = true)(None)
                    from = Nil
                    next
                  case _ =>
                    val next = from.head
                    from = from.tail
                    next
                }).map(CheckTypes.visit(_)(this))
                p
              },
              isRef = false
            )(None)
          ),
          Nil
        )
        lambda
      }
    )
  }

  def currentSubstitution: Option[EirSubstituteTransaction] =
    _substitutions.headOption

  def rollbackTo(s: Option[EirSubstituteTransaction]): Unit = {
    val save = s.map(_substitutions.indexOf(_)).getOrElse(_substitutions.size)
    assert(save >= 0)
    _substitutions.slice(0, save).foreach(deactivate)
  }

  // naively filters out partial specializations
  def checked: Map[EirSpecializable, List[EirSpecialization]] = {
    parent
      .map(_.checked)
      .getOrElse(_checked collect { case (sp, contexts) =>
        (sp, contexts.collect({ case (_, Some(sp)) => sp }).distinct)
      })
  }

  def enterNode(n: EirNode): Unit = {
    stack.push(n)
  }

  private var _templates
      : Map[(EirSpecializable, List[EirType]), EirTemplatedType] = Map()

  def getTemplatedType(
      s: EirSpecializable,
      args: List[EirType]
  ): EirTemplatedType = {
    if (_templates.contains((s, args))) _templates((s, args))
    else {
      val ty = EirTemplatedType(None, assertValid[EirType](s), args)
      _templates += ((s, args) -> ty)
      ty
    }
  }

  def getTemplatedType(
      t: EirTemplatedType,
      args: List[EirType]
  ): EirTemplatedType = {
    getTemplatedType(
      Find.uniqueResolution[EirSpecializable](t.base),
      args
    )
  }

  def makeDistinct(
      s: EirSpecialization,
      args: List[EirType]
  ): EirSpecialization = {
    s match {
      case t: EirTemplatedType => getTemplatedType(t, args)
      case _                   => s
    }
  }

  def getTupleType(tys: Iterable[EirType]): EirTupleType = TupleFactory(tys)

  def start(c: Context): Unit = _contexts.push(c)
  def stop(c: Context): Unit = assert(_contexts.pop() == c)
  def current: Option[Context] = _contexts.headOption
  def cache(n: EirNode, t: EirType): EirType = {
    current.foreach(c => _cache += ((c, n) -> t))
    t
  }
  def avail(n: EirNode): Option[EirType] =
    current.flatMap(c => _cache.get((c, n)))

  def mkCheckContext(
      s: EirSpecializable,
      sp: Option[EirSpecialization]
  ): Option[Context] = {
    val tyCtx = parent.getOrElse(this)
    val checked = tyCtx._checked.getOrElse(s, Nil)
    val member = immediateAncestor[EirMember]
    val isProxy = member.exists(_.hasAnnotation("proxy"))
    val ctx = (
      {
        val base = member.map(_.base)
        Option
          .when(isProxy)({
            tyCtx.ancestor[EirProxy].find(x => base.contains(x.base))
          })
          .flatten
          .orElse(base)
      },
      sp
    )
    Option.unless(checked.contains(ctx))({
      tyCtx._checked += (s -> (checked :+ ctx))
      ctx
    })
  }

  def trySpecialize(s: EirSpecializable): Option[EirSpecialization] = {
    specialization.flatMap(trySpecialize(s, _))
  }

  private var _synthesized
      : Map[List[EirResolvable[EirType]], EirSyntheticSpecialization] = Map()

  def synthesize(types: List[EirResolvable[EirType]]): EirSpecialization = {
    _synthesized.getOrElse(
      types, {
        val sp = EirSyntheticSpecialization(types)
        _synthesized += (types -> sp)
        sp
      }
    )
  }

  def checkPredicate(opt: Option[EirExpressionNode]): Boolean = {
    opt.map(StaticEvaluator.evaluate(_)(this)).forall(_.toBoolean)
  }

  def checkPredicate(pred: EirPredicated): Boolean =
    checkPredicate(pred.predicate)

  def trySpecialize(
      s: EirSpecializable,
      spec: EirSpecialization,
      specTypes: Option[List[EirType]] = None
  ): Option[EirSpecialization] = {
    val args = specTypes.getOrElse {
      spec.types.map(CheckTypes.visit(_)(this))
    }
    val types = s.accepts(spec)(this)
    Option
      .when(types.forall(_.isDefined))({
        if (types.length == spec.types.length) spec
        else synthesize(types.map(_.get))
      })
      .map(enter(s, _, args))
      .filter(sp => {
        checkPredicate(s) || {
          leave(sp); false
        }
      })
  }

  def specialize(
      s: EirSpecializable,
      sp: EirSpecialization,
      spTypes: Option[List[EirType]] = None
  ): EirSpecialization = {
    trySpecialize(s, sp, spTypes).getOrElse(Errors.missingSpecialization(sp))
  }

  private def enter(
      s: EirSpecializable,
      sp: EirSpecialization,
      args: List[EirType]
  ): EirSpecialization = {
    Option
      .unless(s.sameAs(args))({
        activate(EirSubstituteTransaction(s, makeDistinct(sp, args))).sp
      })
      .orNull
  }

  def leave(ours: EirSpecialization): Unit = {
    if (ours != null) {
      _substitutions.find(ours == _.sp).foreach(deactivate)
    }
  }

  def specialization: Option[EirSpecialization] = {
    def canUse(x: EirSpecialization): Boolean = {
      x.types.nonEmpty && !_substitutions.exists(t => x.eq(t.sp))
    }

    stack collectFirst {
      case x: EirSpecialization if canUse(x) => x
    }
  }

  def templateZipArgs(
      s: EirSpecializable,
      sp: EirSpecialization
  ): List[(EirTemplateArgument, EirResolvable[EirType])] = {
    if (sp.types.length < s.templateArgs.length) {
      s.templateArgs.zip(sp.types ++ {
        s.templateArgs
          .slice(sp.types.length, s.templateArgs.length)
          .map(x => {
            x.defaultValue.getOrElse(Errors.missingType(x))
          })
      })
    } else {
      val (init, last) = (s.templateArgs.init, s.templateArgs.last)
      if (last.isPack) {
        init.zip(sp.types) :+ {
          (
            last,
            sp.types
              .slice(init.length, sp.types.length)
              .toTupleType(allowUnit = true)(None)
          )
        }
      } else {
        (init :+ last).zip(sp.types)
      }
    }
  }

  def findSubstitution(s: EirSpecializable): Option[EirSpecialization] = {
    _substitutions.reverse
      .find(x =>
        (x.s, s) match {
          // NOTE EirLambdaExpression does not have template arguments so it's not considered here
          // case (a: EirProxy, b: EirClass) => a.base == b
          // case (a: EirProxy, b: EirTrait) => a.base == b
          case (a: EirLambdaType, b: EirFunction) =>
            a.templateArgs == b.templateArgs
          case (a, b) => a == b
        }
      )
      .map(_.sp)
  }

  def hasSubstitution(
      x: EirTemplateArgument
  ): Option[EirResolvable[EirType]] = {
    _substitutions.reverse
      .flatMap(x => templateZipArgs(x.s, x.sp))
      .collectFirst({
        case (arg, ty) if x == arg => ty
      })
  }

  def leaveWith(t: EirType): EirType = {
    stack.pop()
    t
  }

  def alreadyLeft(n: EirNode): Boolean = !stack.headOption.contains(n)

  def immediateAncestor[T: ClassTag]: Option[T] = {
    Option
      .when(stack.length >= 2)(stack(1) match {
        case x: T => Some(x)
        case _    => None
      })
      .flatten
  }

  def ancestor[T: ClassTag]: Option[T] = stack.collectFirst { case t: T => t }

  def ancestor[T: ClassTag](fn: T => Boolean): Option[T] =
    stack.collectFirst { case t: T if fn(t) => t }

  def immediateAncestor[T: ClassTag](of: EirNode): Option[T] = {
    val idx = stack.indexOf(of)
    Option.when(idx >= 1)(stack(idx + 1)).collect { case t: T => t }
  }

  def popUntil(node: EirNode): Unit = stack.popWhile(node != _)

  type ResolvableType = EirResolvable[EirType]

  type LambdaCharacteristics = (
      List[ResolvableType],
      ResolvableType,
      List[EirTemplateArgument],
      Option[EirExpressionNode]
  )

  private val lambdaBank: mutable.Map[
    LambdaCharacteristics,
    EirLambdaType
  ] = mutable.Map()

  def lambdaWith(
      from: List[ResolvableType],
      to: ResolvableType,
      args: List[EirTemplateArgument] = Nil,
      predicate: Option[EirExpressionNode] = None
  ): EirLambdaType = {
    val characteristics = (from, to, args, predicate)

    if (!lambdaBank.contains(characteristics)) {
      lambdaBank.put(
        characteristics,
        EirLambdaType(None, from, to, args, predicate)
      )
    }

    lambdaBank(characteristics)
  }
}
