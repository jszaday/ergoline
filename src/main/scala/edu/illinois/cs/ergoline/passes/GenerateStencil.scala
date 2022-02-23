package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.{Segmentation, Stencil}
import edu.illinois.cs.ergoline.ast.{
  EirCStyleHeader,
  EirDeclaration,
  EirForAllHeader,
  EirForLoop,
  EirFunction,
  EirFunctionArgument,
  EirNamedNode,
  EirNode,
  EirWhileLoop
}

object GenerateStencil {
  val payloadName = "__payload__"
  val payloadType = "hypercomm::tasking::task_payload"

  def payloadHeader(name: String): String = {
    s"$name($payloadType &&$payloadName)"
  }

  def visitFields(
      it: Iterable[EirNamedNode]
  )(implicit ctx: CodeGenerationContext): Unit = {
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
  )(implicit ctx: CodeGenerationContext): Unit = {
    val arguments = "__arguments__"
    val names = Seq(GenerateProxies.asyncFuture) ++ it.map(ctx.nameFor(_))
    ctx << "auto" << arguments << "=" << "std::forward_as_tuple(" << (names, ",") << ");"
    ctx << s"hypercomm::flex::pup_unpack($arguments, $payloadName.data, std::move($payloadName.src));"
  }

  def visitSerialBlock(x: Segmentation.SerialBlock, inline: Boolean = false)(
      implicit ctx: CodeGenerationContext
  ): Unit = {
    if (!inline) {
      ctx << "void" << nameFor(x) << "(void)" << "{"
    }

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

  def visit(
      construct: Segmentation.Construct
  )(implicit ctx: CodeGenerationContext): Unit = {
    construct match {
      case x: Segmentation.SerialBlock => visitSerialBlock(x)
      case x: Segmentation.Clause      => visitClause(x)
      case x: Segmentation.Loop        => visitLoop(x)
      case _                           => ???
    }
  }

  def visitDeclaration(node: EirNode)(implicit
      ctx: CodeGenerationContext
  ): Unit = {}

  def visitLoopInitializer(
      loop: Segmentation.Loop,
      initializer: EirNode,
      name: String
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "void" << nameFor(loop) << "(void)" << "{"
    visitDeclaration(initializer)
    ctx << s"$name();"
    ctx << "}"
  }

  def visitLoop(
      loop: Segmentation.Loop
  )(implicit ctx: CodeGenerationContext): Unit = {
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
      condition.foreach(GenerateCpp.visitExpression(_))
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
  )(implicit ctx: CodeGenerationContext): Unit = {
    clause.successors.foreach(visit(_))
  }

  def continuation(
      construct: Segmentation.Construct
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << s"${nameFor(construct)}();"
  }

  def visit(fn: EirFunction)(implicit ctx: CodeGenerationContext): Unit = {
    val (res, graph) = Registry.instance[Stencil.Pass].apply(fn)
    val taskName = s"${fn.name}_task"
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
