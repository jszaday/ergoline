package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast.literals.EirIntegerLiteral
import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.{globals, passes}
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.{FullyResolve, Registry}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

import scala.annotation.tailrec
import scala.collection.mutable

object Stencil {
  class Loop {
    var dimensions: Option[List[EirExpressionNode]] = None
    var reductions: Map[EirDeclaration, (EirAssignment, String)] = Map()

    def +:=(x: Option[List[EirExpressionNode]]): Unit = {
      if (x.nonEmpty) {
        assert(dimensions.isEmpty || x == dimensions)
        dimensions = x
      }
    }

    override def toString: String = {
      val builder = new StringBuilder()
      builder.append(super.toString)
      builder.append("(dimensions=")
      builder.append(this.dimensions.toString)
      builder.append(", reductions=")
      reductions.map(_.toString).foreach(builder.append)
      builder.append(")")
      builder.toString()
    }
  }

  class Context {
    var boundaries: Map[EirDeclaration, EirExpressionNode] = Map()
    var dimensions: Map[EirDeclaration, List[EirExpressionNode]] = Map()
    var halos: Map[EirDeclaration, Int] = Map()
    var loops: Map[EirBlock, Loop] = Map()
    var loopStack: mutable.Stack[Loop] = new mutable.Stack
  }

  def visit(ctx: Context, declaration: EirDeclaration): Unit = {
    declaration.initialValue match {
      case Some(rhs) => extractDimensions(ctx, rhs).foreach(dims =>
          ctx.dimensions += (declaration -> dims)
        )
      case None =>
    }
  }

  def extractDimensions(
      ctx: Context,
      expression: EirExpressionNode
  ): Option[List[EirExpressionNode]] = {
    expression match {
      case call: EirFunctionCall =>
        isClone(ctx, call.target).orElse(isFill(call))
      case create: EirNew if isArray(create.target) => Some(create.args)
      case _                                        => None
    }
  }

  def isFill(call: EirFunctionCall): Option[List[EirExpressionNode]] = {
    def isFillCall(resolvable: EirResolvable[_]): Boolean = {
      isSymbolInList(resolvable, List("fill"))
    }

    call.target match {
      case EirScopedSymbol(target: EirResolvable[_], pending)
          if isArray(target) && isFillCall(pending) =>
        call.args.map(_.expr).headOption.collect {
          case tuple: EirTupleExpression => tuple.expressions
          case expression                => List(expression)
        }
      case _ => None
    }
  }

  def isClone(
      ctx: Context,
      expression: EirExpressionNode
  ): Option[List[EirExpressionNode]] = {
    def isCloneCall(resolvable: EirResolvable[_]): Boolean = {
      isSymbolInList(resolvable, List("clone"))
    }

    expression match {
      case EirScopedSymbol(target: EirResolvable[_], pending)
          if isCloneCall(pending) =>
        Find
          .resolutions[EirDeclaration](target)
          .headOption
          .flatMap(ctx.dimensions.get)
      case _ => None
    }
  }

  @tailrec
  def isArray(resolvable: EirResolvable[_]): Boolean = {
    resolvable match {
      case x: EirSpecializedSymbol[_] => isArray(x.symbol)
      case x: EirSymbol[_]            => isSymbolInList(x, List("array"))
      case _                          => false
    }
  }

  def isSymbolInList(
      resolvable: EirResolvable[_],
      list: List[String]
  ): Boolean = {
    resolvable match {
      case EirSymbol(_, List(name)) => list.contains(name)
      case _                        => false
    }
  }

  def isBoundingCall(resolvable: EirResolvable[_]): Boolean = {
    isSymbolInList(resolvable, List("pad"))
  }

  def visit(
      ctx: Context,
      ref: EirArrayReference
  ): Unit = {
    val halo = {
      val args = ref.args.collect { case EirIntegerLiteral(x) => Math.abs(x) }
      Option.when(args.nonEmpty)(args.max)
    }

    ref.target match {
      case x: EirSymbol[_] => Find
          .resolutions[EirDeclaration](x)
          .foreach(declaration => {
            halo.foreach(i => {
              ctx.halos += (declaration -> ctx.halos
                .get(declaration)
                .map(Math.max(_, i))
                .getOrElse(i))
            })

            ctx.loopStack.headOption.foreach(loop => {
              loop +:= ctx.dimensions.get(declaration)
            })
          })
      case _ =>
    }
  }

  def visit(
      ctx: Context,
      assignment: EirAssignment
  ): Unit = {
    val lhs = assignment.lval match {
      case resolvable: EirResolvable[_] => Find
          .resolutions[EirDeclaration](resolvable)
          .filterNot(declaration => isArray(declaration.declaredType))
      case _ => Nil
    }
    val rhs = Find
      .descendant(
        assignment.rval,
        node => Some(node.isInstanceOf[EirResolvable[_]])
      )
      .map(_.asInstanceOf[EirResolvable[EirNode]])
      .flatMap(Find.resolutions[EirDeclaration])
      .toList
    val reduction = lhs.exists(rhs.contains(_))
    val combiner =
      if (assignment.op == "=") {
        if (reduction) {
          assignment.rval match {
            case call: EirFunctionCall => Some(call.target.toString)
            case _                     => None
          }
        } else {
          None
        }
      } else {
        assert(!reduction)
        Some(assignment.op.substring(0, assignment.op.length - 2))
      }

    combiner.foreach(op => {
      lhs.map(declaration =>
        ctx.loopStack.headOption.foreach(loop => {
          loop.reductions += (declaration -> (assignment, op))
        })
      )
    })

    assignment.children.foreach(visit(ctx, _))
  }

  def visitForEach(ctx: Context, call: EirFunctionCall): Unit = {
    call.args
      .map(_.expr)
      .collectFirst { case x: EirClosure => x.block }
      .foreach(block => {
        val loop = new Loop

        ctx.loops += (block -> loop)
        ctx.loopStack.push(loop)

        visit(ctx, block)

        assert(loop == ctx.loopStack.pop())
      })
  }

  def visit(ctx: Context, call: EirFunctionCall): Unit = {
    call.target match {
      case x: EirScopedSymbol[_] => if (isBoundingCall(x.pending)) {
          x.target match {
            case target: EirResolvable[_] => Find
                .resolutions[EirDeclaration](target)
                .foreach(declaration => ctx.boundaries += (declaration -> call))
            case _ =>
          }
        }
      case x: EirSymbol[_] => if (isSymbolInList(x, List("foreach"))) {
          visitForEach(ctx, call)
        }
      // else if (isSymbolInList(x, List("boundary"))) {}
      case _ =>
    }
  }

  def visit(ctx: Context, node: EirNode): Unit = {
    node match {
      case x: EirDeclaration    => visit(ctx, x)
      case x: EirFunctionCall   => visit(ctx, x)
      case x: EirArrayReference => visit(ctx, x)
      case x: EirAssignment     => visit(ctx, x)
      case _                    => node.children.foreach(visit(ctx, _))
    }
  }

  def visit(fn: EirFunction): Unit = {
    val blk = fn.body match {
      case Some(x) => x
      case None    => Errors.missingBody(fn)
    }

    fn.body = None
    fn.annotations ++= List(EirAnnotation("system", Map()))
    fn.returnType =
      EirTemplatedType(None, globals.futureType, List(fn.returnType))

    val ctx = new Context()
    blk.children.foreach(visit(ctx, _))

    ctx.boundaries.foreach(println(_))
    ctx.dimensions.foreach(println(_))
    ctx.halos.foreach(println(_))
    ctx.loops.foreach(println(_))
  }

  class Pass extends passes.Pass {
    override def phase: Phase = Phase.Load

    override def after: Seq[passes.Pass] = {
      Seq(Registry.instance[FullyResolve])
    }

    override def annotations: Seq[String] = Seq("stencil")

    override def apply(n: EirNode): Unit = {
      n match {
        case fn: EirFunction => visit(fn)
        case _               =>
      }
    }
  }
}
