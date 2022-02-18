package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast.{
  EirAnnotation,
  EirDeclaration,
  EirExpressionNode,
  EirFunction,
  EirFunctionCall,
  EirNew,
  EirNode,
  EirScopedSymbol,
  EirSymbol,
  EirTupleExpression
}
import edu.illinois.cs.ergoline.{globals, passes}
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.{FullyResolve, Registry}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors

object Stencil {
  class Context {
    var boundaries: Map[EirDeclaration, EirExpressionNode] = Map()
    var dimensions: Map[EirDeclaration, List[EirExpressionNode]] = Map()
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

  def isArray(resolvable: EirResolvable[_]): Boolean = {
    true
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
      case x: EirSymbol[_] =>
      case _               =>
    }
  }

  def visit(ctx: Context, node: EirNode): Unit = {
    node match {
      case x: EirDeclaration  => visit(ctx, x)
      case x: EirFunctionCall => visit(ctx, x)
      case _                  => node.children.foreach(visit(ctx, _))
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

    ctx.dimensions.foreach(println(_))
    ctx.boundaries.foreach(println(_))
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
