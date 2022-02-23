package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.{Segmentation, Stencil}
import edu.illinois.cs.ergoline.ast._

import scala.language.implicitConversions

object GenerateStencil {
  val payloadName = "__payload__"
  val payloadType = "hypercomm::tasking::task_payload"

  case class Context(info: Stencil.Context, ctx: CodeGenerationContext)

  implicit def ctx2ctx(ctx: Context): CodeGenerationContext = ctx.ctx

  def payloadHeader(name: String): String = {
    s"$name($payloadType &&$payloadName)"
  }

  def visitFields(
      it: Iterable[EirNamedNode]
  )(implicit ctx: Context): Unit = {
    it.foreach(node => {
      val ty = CheckTypes.stripReference(node match {
        case x: EirDeclaration => CheckTypes.visitDeclaration(x)(ctx.tyCtx)
        case x: EirFunctionArgument =>
          CheckTypes.visitFunctionArgument(x)(ctx.tyCtx)
      })
      ctx << ctx.typeFor(ty, Some(node)) << ctx.nameFor(node) << ";"
    })
  }

  def visitArguments(
      it: Iterable[EirFunctionArgument]
  )(implicit ctx: Context): Unit = {
    val arguments = "__arguments__"
    val names = Seq(GenerateProxies.asyncFuture) ++ it.map(ctx.nameFor(_))
    ctx << "auto" << arguments << "=" << "std::forward_as_tuple(" << (names, ",") << ");"
    ctx << s"hypercomm::flex::pup_unpack($arguments, $payloadName.data, std::move($payloadName.src));"
  }

  def visitSerialBlock(x: Segmentation.SerialBlock, inline: Boolean = false)(
      implicit ctx: Context
  ): Unit = {
    if (!inline) {
      ctx << "void" << nameFor(x) << "(void)" << "{"
    }

    x.slst.foreach(visit(_))
    x.successors.foreach(ctx << continuation(_))

    if (!inline) {
      ctx << "}"

      x.successors.foreach(visit(_))
    }
  }

  val defaultSuffix = "begin"

  def nameFor(
      x: Segmentation.Construct,
      suffix: String = defaultSuffix
  ): String = {
    s"block_${x.id}_$suffix"
  }

  def visitForEach(call: EirFunctionCall)(implicit
      ctx: Context
  ): Unit = {}

  def visit(node: EirNode)(implicit ctx: Context): Unit = {
    node match {
      case x: EirFunctionCall if Stencil.isForEach(x) => visitForEach(x)
      case x: EirDeclaration                          => visitDeclaration(x)
      case x: EirBlock                                => ctx << "{" << x.children.foreach(visit(_)) << "}"
      case _ =>
        CheckTypes.visit(node)(ctx.tyCtx)
        GenerateCpp.visit(node)(ctx.ctx)
        ctx << ";"
    }
  }

  def visit(
      construct: Segmentation.Construct
  )(implicit ctx: Context): Unit = {
    construct match {
      case x: Segmentation.SerialBlock => visitSerialBlock(x)
      case x: Segmentation.Clause      => visitClause(x)
      case x: Segmentation.Loop        => visitLoop(x)
      case _                           => ???
    }
  }

  def visitDeclaration(node: EirNode)(implicit
      ctx: Context
  ): Unit = node match {
    case x: EirDeclaration => visitDeclaration(x)
    case _                 => ???
  }

  def visitDeclaration(declaration: EirDeclaration)(implicit
      ctx: Context
  ): Unit = {
    declaration.initialValue match {
      case Some(x) =>
        CheckTypes.visit(x)(ctx.tyCtx)
        ctx << ctx.nameFor(declaration) << "=" << GenerateCpp.visit(x)(
          ctx.ctx
        ) << ";"
      case None =>
    }
  }

  def visitLoopInitializer(
      loop: Segmentation.Loop,
      initializer: EirNode,
      name: String
  )(implicit ctx: Context): Unit = {
    ctx << "void" << nameFor(loop) << "(void)" << "{"
    visitDeclaration(initializer)
    ctx << s"$name();"
    ctx << "}"
  }

  def visitLoop(
      loop: Segmentation.Loop
  )(implicit ctx: Context): Unit = {
    val condition = loop.node match {
      case x: EirWhileLoop                      => Some(x.condition)
      case EirForLoop(_, x: EirCStyleHeader, _) => x.test
      case _                                    => ???
    }

    val initializer = Option(loop.node).collect { case x: EirForLoop =>
      x.header.declaration
    }.flatten
    val suffix = initializer.map(_ => "header").getOrElse(defaultSuffix)
    initializer.foreach(visitLoopInitializer(loop, _, suffix))

    ctx << "void" << nameFor(loop, suffix) << "(void)" << "{"
    ctx << "if" << "(" << {
      condition.foreach(CheckTypes.visitExpression(_)(ctx.tyCtx))
      condition.foreach(GenerateCpp.visitExpression(_)(ctx.ctx))
    } << ")" << "{"
    loop.body.foreach(continuation(_))
    ctx << "}" << "else" << "{"
    loop.successors.foreach(continuation(_))
    ctx << "}"
    ctx << "}"

    (loop.body ++ loop.successors).foreach(visit(_))
  }

  def visitClause(
      clause: Segmentation.Clause
  )(implicit ctx: Context): Unit = {
    clause.successors.foreach(visit(_))
  }

  def continuation(
      construct: Segmentation.Construct
  )(implicit ctx: Context): Unit = {
    ctx << s"${nameFor(construct)}();"
  }

  def visit(fn: EirFunction)(implicit cgen: CodeGenerationContext): Unit = {
    val (res, graph) = Registry.instance[Stencil.Pass].apply(fn)
    val taskName = s"${fn.name}_task"
    implicit val ctx: Context = Context(res, cgen)
    ctx << "class" << taskName << ":" << "public" << "hypercomm::tasking::task<" << taskName << ">" << "{"
    ctx << "public: // ;"
    visitFields(res.declarations)
    ctx << taskName << "(PUP::reconstruct) {}"
    ctx << payloadHeader(taskName) << "{"
    visitArguments(fn.functionArgs)
    graph match {
      case Some(x: Segmentation.SerialBlock) =>
        visitSerialBlock(x, inline = true)
        ctx << "}"
        x.successors.foreach(visit(_))
      case Some(x) =>
        continuation(x)
        ctx << "}"
        visit(x)
      case None => ctx << "}"
    }
    ctx << "};"
  }
}
