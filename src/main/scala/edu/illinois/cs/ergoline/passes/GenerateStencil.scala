package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.Stencil.ClauseKind
import edu.illinois.cs.ergoline.analysis.{Segmentation, Stencil}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}

import scala.collection.{View, mutable}
import scala.language.implicitConversions

object GenerateStencil {
  val payloadName = "__payload__"
  val payloadType = "hypercomm::tasking::task_payload"
  val numNeighbors = "num_neighbors"

  case class Context(
      info: Stencil.Context,
      ctx: CodeGenerationContext,
      taskName: String
  ) {
    var counters: List[EirSdagWhen] = Nil
    var stack: mutable.Stack[(Segmentation.Construct, String)] =
      new mutable.Stack

    def counterFor(x: EirSdagWhen): String = {
      s"__counter_${counters.indexOf(x)}__"
    }
  }

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

  def unpack(
      names: Iterable[String]
  )(implicit ctx: Context): Unit = {
    val arguments = "__arguments__"
    ctx << "auto" << arguments << "=" << "std::forward_as_tuple(" << (names, ",") << ");"
    ctx << s"hypercomm::flex::pup_unpack($arguments, $payloadName.data, std::move($payloadName.src));"
  }

  def visitArguments(
      it: Iterable[EirFunctionArgument]
  )(implicit ctx: Context): Unit = {
    unpack(Seq(GenerateProxies.asyncFuture) ++ it.map(ctx.nameFor(_)))
  }

  def makeContinuation(x: Segmentation.Construct)(implicit
      ctx: Context
  ): Unit = {
    if (x.successors.isEmpty) {
      ctx.stack.lastOption.foreach { case (a, b) =>
        ctx << s"${nameFor(a, b)}();"
      }
    } else {
      x.successors.foreach(continuation)
    }
  }

  def visitSerialBlock(x: Segmentation.SerialBlock, inline: Boolean = false)(
      implicit ctx: Context
  ): Unit = {
    if (!inline) {
      ctx << "void" << nameFor(x) << "(void)" << "{"
    }

    x.slst.foreach(visit(_))
    makeContinuation(x)

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
    makeContinuation(loop)
    ctx << "}"
    ctx << "}"

    ctx.stack.push((loop, suffix))
    loop.body.foreach(visit(_))
    ctx.stack.pop()

    loop.successors.foreach(visit(_))
  }

  def sendHalo(array: EirDeclaration, offset: String)(implicit
      ctx: Context
  ): Unit = {
    ctx << s"// send [_, $offset] of ${array.name} ;"
  }

  def lastDimSize(array: EirDeclaration)(implicit ctx: Context): String = {
    ctx.info.dimensions(array).lastOption.map(_.toString).getOrElse(???)
  }

  def declarationsFor(call: EirFunctionCall): View[EirDeclaration] = {
    call.args.view
      .map(_.expr)
      .map(_.asInstanceOf[EirResolvable[EirNamedNode]])
      .map(Find.uniqueResolution[EirDeclaration])
  }

  def beginBoundary(
      clause: Segmentation.Clause,
      call: EirFunctionCall
  )(implicit ctx: Context): Unit = {
    ctx << ctx.counterFor(clause.node) << "=" << "0;"

    val decls = declarationsFor(call).toList

    ctx << "if (this->has_left())" << "{"
    decls.foreach(sendHalo(_, "1"))
    ctx << "}"

    ctx << "if (this->has_right())" << "{"
    decls.foreach(x => sendHalo(x, lastDimSize(x) + "-1"))
    ctx << "}"
  }

  def endBoundary(
      clause: Segmentation.Clause,
      call: EirFunctionCall,
      methodRef: String
  )(implicit ctx: Context): Unit = {
    ctx << "if (++" << ctx.counterFor(
      clause.node
    ) << ">=" << s"this->$numNeighbors()" << ")" << "{"
    makeContinuation(clause)
    ctx << "}" << "else" << "{"
    ctx << "this->suspend<" << methodRef << ">();"
    ctx << "}"
  }

  def ckReductionFor(t: EirType, op: EirExpressionNode)(implicit
      ctx: Context
  ): String = {
    "CkReduction::" + {
      op match {
        case EirSymbol(_, List("math", "max")) => s"max_${ctx.nameFor(t)}"
        case _                                 => ???
      }
    }
  }

  def beginReduction(
      call: EirFunctionCall,
      methodRef: String
  )(implicit ctx: Context): Unit = {
    val target = declarationsFor(call).head
    val targetType =
      CheckTypes.stripReference(CheckTypes.visit(target)(ctx.tyCtx))
    val operator = call.args(1).expr
    ctx << "this->all_reduce<" << methodRef << ">(" << ctx.nameFor(
      target
    ) << "," << ckReductionFor(targetType, operator) << ");"
  }

  def endReduction(
      clause: Segmentation.Clause,
      call: EirFunctionCall
  )(implicit ctx: Context): Unit = {
    val target = declarationsFor(call).head
    unpack(Seq(ctx.nameFor(target)))
    makeContinuation(clause)
  }

  def visitClause(
      clause: Segmentation.Clause
  )(implicit ctx: Context): Unit = {
    val continuation = nameFor(clause, "continuation")
    val methodRef = s"&${ctx.taskName}::$continuation"
    val kind = ctx.info.classifications(clause.node)
    val call = clause.node.body match {
      case Some(x: EirFunctionCall) => x
      case _                        => ???
    }

    ctx << "void" << nameFor(clause) << "(void)" << "{"
    kind match {
      case ClauseKind.Boundary  => beginBoundary(clause, call)
      case ClauseKind.Reduction => beginReduction(call, methodRef)
      case _                    =>
    }
    ctx << "this->suspend<" << methodRef << ">();"
    ctx << "}"

    ctx << "void" << payloadHeader(continuation) << "{"
    kind match {
      case ClauseKind.Boundary  => endBoundary(clause, call, methodRef)
      case ClauseKind.Reduction => endReduction(clause, call)
      case _                    =>
    }
    ctx << "}"

    clause.successors.foreach(visit(_))
  }

  def continuation(
      construct: Segmentation.Construct
  )(implicit ctx: Context): Unit = {
    ctx << s"${nameFor(construct)}();"
  }

  def visit(fn: EirFunction)(implicit cgn: CodeGenerationContext): Unit = {
    val (res, graph) = Registry.instance[Stencil.Pass].apply(fn)
    val taskName = s"${fn.name}_task"
    implicit val ctx: Context = Context(res, cgn, taskName)
    ctx << "class" << taskName << ":" << "public" << "hypercomm::tasking::task<" << taskName << ">" << "{"
    ctx << "public: // ;"
    visitFields(res.declarations)

    res.classifications
      .filter(_._2 == ClauseKind.Boundary)
      .keys
      .foreach(x => {
        ctx.counters :+= x
        ctx << s"int ${ctx.counterFor(x)};"
      })

    ctx << "bool has_left(void) const { return (this->index() > 0); }"
    ctx << "bool has_right(void) const {" << {
      "return ((this->index() + 1) < ergoline::workgroup_size());"
    } << "}"
    ctx << s"int $numNeighbors(void) const {" << {
      "return (int)this->has_left() + (int)this->has_right();"
    } << "}"

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
