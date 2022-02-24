package edu.illinois.cs.ergoline.analysis

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.EirIntegerLiteral
import edu.illinois.cs.ergoline.ast.types.{
  EirNamedType,
  EirTemplatedType,
  EirType
}
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.passes.{
  CheckEnclose,
  FullyResolve,
  GenerateProxies,
  Processes,
  Registry
}
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find}
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.{globals, passes}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

import scala.annotation.tailrec
import scala.collection.mutable

object Stencil {
  object ClauseKind extends Enumeration {
    type ClauseKind = Value
    val Boundary, Reduction, Gather = Value
  }

  class Loop {
    var dimensions: Option[List[EirExpressionNode]] = None
    var reductions: Map[EirDeclaration, (EirAssignment, EirExpressionNode)] =
      Map()

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

  import ClauseKind._

  class Context {
    var boundaries: Map[EirDeclaration, EirExpressionNode] = Map()
    var declarations: Set[EirNamedNode] = Set()
    var dimensions: Map[EirDeclaration, List[EirExpressionNode]] = Map()
    var halos: Map[EirDeclaration, Int] = Map()
    var loops: Map[EirBlock, Loop] = Map()
    var loopStack: mutable.Stack[Loop] = new mutable.Stack
    var classifications: Map[EirSdagWhen, ClauseKind] = Map()

    def apply[A](f: Loop => A): Option[A] = {
      this.loopStack.headOption.map(f)
    }
  }

  def visit(ctx: Context, declaration: EirDeclaration): Unit = {
    ctx.declarations += declaration

    declaration.initialValue match {
      case Some(rhs) => extractDimensions(ctx, rhs).foreach(dims =>
          ctx.dimensions += (declaration -> dims)
        )
      case None =>
    }

    declaration.children.foreach(visit(ctx, _))
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
    val decl = ref.target match {
      case x: EirSymbol[_] => Find.resolutions[EirDeclaration](x).headOption
      case _               => None
    }

    if (ref.args.isEmpty) {
      val nd = decl.map(ctx.dimensions.get).map(_.size)
      nd.foreach(x => {
        ref.args = (0 to x).map(_ => EirIntegerLiteral(0)(Some(ref))).toList
      })
    }

    val halo = {
      val args = ref.args.collect { case EirIntegerLiteral(x) => Math.abs(x) }
      Option.when(args.nonEmpty)(args.max)
    }

    decl
      .foreach(declaration => {
        halo.foreach(i => {
          ctx.halos += (declaration -> ctx.halos
            .get(declaration)
            .map(Math.max(_, i))
            .getOrElse(i))
        })

        ctx(_ +:= ctx.dimensions.get(declaration))
      })
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
            case call: EirFunctionCall => Some(call.target)
            case _                     => None
          }
        } else {
          None
        }
      } else {
        assert(!reduction)
        Some(
          EirSymbol[EirNamedNode](
            None,
            List(assignment.op.substring(0, assignment.op.length - 2))
          )
        )
      }

    combiner.foreach(op => {
      lhs.map(declaration =>
        ctx(_.reductions += (declaration -> (assignment, op)))
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

        if (loop.reductions.nonEmpty) {
          def convertReductions(
              it: Iterable[(EirDeclaration, (EirAssignment, EirExpressionNode))]
          ): List[EirCallArgument] = {
            it.flatMap { case (decl, (_, op)) =>
              List(EirSymbol[EirDeclaration](Some(block), List(decl.name)), op)
            }.map(EirCallArgument(_, isRef = false)(None))
              .toList
          }

          call.parent
            .collect { case block: EirBlock => block }
            .foreach(block => {
              val pos = block.findPositionOf(call)
              val clause = EirSdagWhen(Nil, None, None)(call.parent)
              ctx.classifications += (clause -> ClauseKind.Reduction)
              clause.body = Some(
                EirFunctionCall(
                  None,
                  EirSymbol(None, List("all_reduce")),
                  convertReductions(loop.reductions),
                  Nil
                )
              )
              pos.foreach(x => block.insertAt(x + 1, clause))
            })
        }
      })
  }

  def visitBoundary(ctx: Context, call: EirFunctionCall): Unit = {
    call.parent.foreach(node => {
      val clause = EirSdagWhen(Nil, None, None)(call.parent)
      ctx.classifications += (clause -> ClauseKind.Boundary)
      clause.body = Some(call)
      assert(node.replaceChild(call, clause))
    })
  }

  def isForEach(symbol: EirSymbol[_]): Boolean = {
    isSymbolInList(symbol, List("foreach"))
  }

  def isForEach(call: EirFunctionCall): Boolean = {
    call.target match {
      case x: EirSymbol[_] => isForEach(x)
      case _               => false
    }
  }

  def visit(ctx: Context, call: EirFunctionCall): Unit = {
    val visited = call.target match {
      case x: EirScopedSymbol[_] =>
        if (isBoundingCall(x.pending)) {
          x.target match {
            case target: EirResolvable[_] => Find
                .resolutions[EirDeclaration](target)
                .foreach(declaration => ctx.boundaries += (declaration -> call))
            case _ =>
          }

          call.parent
            .to[EirBlock]
            .foreach(block => {
              block
                .findPositionOf(call)
                .foreach(pos => {
                  block.children = block.children.patch(pos, Nil, 1)
                })
            })
        }
        false
      case x: EirSymbol[_] =>
        val foreach = isForEach(x)
        if (foreach) {
          visitForEach(ctx, call)
        } else if (isSymbolInList(x, List("boundary"))) {
          visitBoundary(ctx, call)
        }
        foreach
      case _ => false
    }

    if (!visited) {
      call.args.foreach(visit(ctx, _))
    }
  }

  def visit(ctx: Context, ret: EirReturn): Unit = {
    // TODO ( support more complicated expressions here )
    val returnValue = ret.expression match {
      case resolvable: EirResolvable[_] => Find
          .resolutions[EirDeclaration](resolvable)
          .headOption
      case _ => None
    }

    if (returnValue.exists(ctx.dimensions.contains)) {
      ret.parent.foreach(parent => {
        val clause = EirSdagWhen(Nil, None, None)(ret.parent)
        val arg = EirCallArgument(ret.expression, isRef = false)(None)
        ctx.classifications += (clause -> ClauseKind.Gather)
        clause.body = Some(
          EirFunctionCall(None, EirSymbol(None, List("gather")), List(arg), Nil)
        )
        parent.replaceChild(ret, clause)
      })
    }
  }

  def visit(ctx: Context, node: EirNode): Unit = {
    node match {
      case x: EirDeclaration    => visit(ctx, x)
      case x: EirFunctionCall   => visit(ctx, x)
      case x: EirArrayReference => visit(ctx, x)
      case x: EirAssignment     => visit(ctx, x)
      case x: EirReturn         => visit(ctx, x)
      case _                    => node.children.foreach(visit(ctx, _))
    }
  }

  private def update(ctx: Context, fn: EirFunction): Unit = {
    val returnType = fn.returnType
    val futureType =
      EirTemplatedType(Some(fn), globals.futureType, List(returnType))
    returnType.parent = Some(futureType)
    fn.returnType = futureType

    val block = EirBlock(Some(fn), Nil)
    fn.body = Some(block)

    val decl = EirDeclaration(
      Some(block),
      isFinal = true,
      GenerateProxies.asyncFuture,
      null,
      None
    )

    ctx.declarations += decl

    val creator = EirFunctionCall(Some(decl), null, Nil, Nil)
    decl.declaredType = EirPlaceholder(Some(decl), None)
    decl.initialValue = Some(creator)
    val symbol =
      EirSpecializedSymbol[EirNamedNode](Some(creator), null, List(returnType))
    creator.target = symbol
    symbol.symbol = EirSymbol[EirClassLike](Some(symbol), List("ck", "future"))

    val ret = EirReturn(Some(block), null)
    val futureSymbol =
      EirSymbol[EirDeclaration](Some(ret), List(GenerateProxies.asyncFuture))
    ret.expression = futureSymbol

    val call = EirFunctionCall(Some(block), null, Nil, Nil)
    call.target = EirSymbol[EirFunction](Some(call), List("launch_"))
    call.args = EirCallArgument(futureSymbol, isRef = false)(Some(call)) +: {
      fn.functionArgs
        .map(x => EirSymbol[EirFunctionArgument](None, List(x.name)))
        .map(x => {
          val arg: EirCallArgument =
            EirCallArgument(x, isRef = false)(Some(call))
          x.parent = Some(arg)
          arg
        })
    }

    block.children = List(decl, call, ret)

    if (fn.implicitArgs.isEmpty) {
      val arg = EirFunctionArgument(
        Some(fn),
        globals.implicitProxyName,
        null,
        isExpansion = false
      )
      arg.declaredType =
        EirSymbol[EirClassLike](Some(symbol), List("ck", "proxy"))
      arg.isImplicit = true
      fn.implicitArgs = List(arg)
    }
  }

  type Result = (Context, Option[Segmentation.Construct])

  def visit(fn: EirFunction): Result = {
    val blk = fn.body match {
      case Some(x) => x
      case None    => Errors.missingBody(fn)
    }

    val ctx = new Context()
    visit(ctx, blk)

    ctx.declarations ++= fn.functionArgs

    val graph = Registry.instance[Segmentation.Pass].apply(fn)
    graph.map(Segmentation.toGraph(_, fn.name)).foreach(println)

    update(ctx, fn)

    ctx.boundaries.foreach(println(_))
    ctx.declarations.foreach(println(_))
    ctx.dimensions.foreach(println(_))
    ctx.halos.foreach(println(_))
    ctx.loops.foreach(println(_))

    (ctx, graph)
  }

  class Pass extends passes.Pass {
    private var _memo: Map[EirFunction, Result] = Map()

    override def phase: Phase = Phase.Load

    override def after: Seq[passes.Pass] = {
      Seq(Registry.instance[FullyResolve])
    }

    def apply(x: EirFunction): Result = {
      Processes.cppIncludes += "hypercomm/tasking/workgroup.hpp"
      Processes.defIncludes += "hypercomm/tasking/tasking.def.h"

      this._memo.getOrElse(
        x, {
          val res = visit(x)
          this._memo += (x -> res)
          res
        }
      )
    }

    override def apply(node: EirNode): Unit = {
      node match {
        case x: EirFunction if x.hasAnnotation("stencil") => apply(x)
        case x: EirClassLike                              => x.members.foreach(member => this(member.member))
        case _                                            =>
      }
    }
  }
}
