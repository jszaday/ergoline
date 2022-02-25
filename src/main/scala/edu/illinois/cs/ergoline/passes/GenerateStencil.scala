package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.Stencil.{ClauseKind, isSymbolInList}
import edu.illinois.cs.ergoline.analysis.{Segmentation, Stencil}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.literals.EirIntegerLiteral
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

import scala.collection.{View, mutable}
import scala.language.implicitConversions

object GenerateStencil {
  val payloadName = "__payload__"
  val payloadType = "hypercomm::tasking::task_payload"
  val numNeighbors = "num_neighbors"
  val hasAbove = "has_neighbor_above"
  val hasBelow = "has_neighbor_below"

  def arrayAbove(name: String) = s"__${name}_above__"
  def arrayBelow(name: String) = s"__${name}_below__"

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
  )(implicit ctx: Context): Iterable[EirDeclaration] = {
    val list = it.toList
    val names = list.map(ctx.nameFor(_))
    val boundaries = list.map {
      Option(_)
        .to[EirDeclaration]
        .filter(x => ctx.info.boundaries.contains(x))
    }
    val zipped = list.zip(names).zip(boundaries)

    zipped.foreach { case ((node, nameFmt), bounds) =>
      val ty = CheckTypes.stripReference(node match {
        case x: EirDeclaration => CheckTypes.visitDeclaration(x)(ctx.tyCtx)
        case x: EirFunctionArgument =>
          CheckTypes.visitFunctionArgument(x)(ctx.tyCtx)
      })

      val tyFmt = ctx.typeFor(ty, Some(node))

      ctx << tyFmt << nameFmt << ";"

      bounds.foreach(_ => {
        ctx << tyFmt << arrayAbove(nameFmt) << ";"
        ctx << tyFmt << arrayBelow(nameFmt) << ";"
      })
    }

    ctx << "void pup(PUP::er& p) {"
    ctx << "auto s = hypercomm::make_serdes(p);"
    zipped.foreach { case ((_, nameFmt), bounds) =>
      ctx << "hypercomm::pup(*s, " << nameFmt << ");"

      bounds.foreach(_ => {
        ctx << "hypercomm::pup(*s, " << arrayAbove(nameFmt) << ");"
        ctx << "hypercomm::pup(*s, " << arrayAbove(nameFmt) << ");"
      })
    }
    ctx << "}"

    boundaries.flatten
  }

  def boundaryValue(node: EirExpressionNode): (String, EirExpressionNode) = {
    node match {
      case EirFunctionCall(_, EirScopedSymbol(_, x), value :: _, _)
          if isSymbolInList(x, List("pad")) => ("pad", value.expr)
    }
  }

  def makeBoundaryInitializer(
      declaration: EirDeclaration,
      boundary: EirExpressionNode
  )(implicit ctx: Context): EirNode = {
    val halo = ctx.info.halos(declaration)
    val size = lastDimSize(declaration)
    val ty = CheckTypes.stripReference(ctx.typeOf(declaration))
    val tyFmt = ctx.nameFor(ty, Some(declaration))
    assert(halo > 0)
    boundaryValue(boundary) match {
      case ("pad", value) => GenerateCpp.CppNode(
          s"$tyFmt::fill(std::make_tuple($halo, $size), $value)"
        )
    }
  }

  def makeBoundaries(
      it: Iterable[EirDeclaration]
  )(implicit ctx: Context): Unit = {
    it.foreach(x => {
      val nameFmt = ctx.nameFor(x)
      val boundary = ctx.info.boundaries(x)
      val initializer = makeBoundaryInitializer(x, boundary)

      ctx << s"if (!this->$hasAbove()) {"
      ctx << arrayAbove(nameFmt) << "=" << GenerateCpp.visit(initializer)(
        ctx.ctx
      ) << ";"
      ctx << "}"

      ctx << s"if (!this->$hasBelow()) {"
      ctx << arrayBelow(nameFmt) << "=" << GenerateCpp.visit(initializer)(
        ctx.ctx
      ) << ";"
      ctx << "}"
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
  ): Unit = {
    val body = call.args
      .map(_.expr)
      .headOption
      .to[EirClosure]
      .map(_.block)
      .getOrElse(???)
    val info = ctx.info.loops(body)
    val dimensions = info.dimensions.getOrElse(???)
    val counters = dimensions.indices.map(i => s"__it_${i}__").toList

    CheckTypes.visit(body)(ctx.tyCtx)

    val declarations = info.references.map(_.target).map {
      case x: EirSymbol[_] => Find.uniqueResolution[EirDeclaration](x)
    }

    info.references.zip(declarations).foreach { case (ref, decl) =>
      val boundary = ctx.info.boundaries.get(decl)
      val args = counters.zip(ref.args).map { case (counter, arg) =>
        GenerateCpp.CppNode(s"($counter + ($arg))")
      }

      if (boundary.nonEmpty) {
        ref.parent.foreach(parent => {
          parent match {
            case x: EirExpressionNode => x.disambiguation = None
            case _                    =>
          }

          assert(
            parent.replaceChild(
              ref, {
                GenerateCpp.CppNode(
                  s"this->${decl.name}_at(${args.map(_.s).mkString(", ")})"
                )
              }
            )
          )
        })
      } else {
        ref.args = args
      }
    }

    counters.zip(dimensions).foreach { case (counter, dim) =>
      ctx << s"for (auto $counter = 0; $counter < ($dim); $counter++) {"
    }

    GenerateCpp.visit(body)(ctx.ctx)

    counters.foreach(_ => {
      ctx << "}"
    })
  }

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

  def sendHalo(
      array: EirDeclaration,
      methodRef: String,
      direction: String,
      offset: String,
      size: Int
  )(implicit
      ctx: Context
  ): Unit = {
    ctx << "auto __slice__ = ergoline::take_slice(" << ctx.nameFor(
      array
    ) << s", $size, " << offset << ");"
    ctx << s"this->send<$methodRef>(this->index() " << direction << ", this->index(), __slice__);"
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
      call: EirFunctionCall,
      methodRef: String
  )(implicit ctx: Context): Unit = {
    ctx << ctx.counterFor(clause.node) << "=" << "0;"

    val decls = declarationsFor(call).toList
    val halos = decls.map(ctx.info.halos(_))

    ctx << s"if (this->$hasAbove())" << "{"
    decls.zip(halos).foreach { case (x, halo) =>
      sendHalo(x, methodRef, "- 1", "0", halo)
    }
    ctx << "}"

    ctx << s"if (this->$hasBelow())" << "{"
    decls.zip(halos).foreach { case (x, halo) =>
      sendHalo(x, methodRef, "+ 1", s"(${lastDimSize(x)}) - $halo", halo)
    }
    ctx << "}"

    ctx << s"if (this->$hasAbove() || this->$hasBelow()) {"
    ctx << "this->suspend<" << methodRef << ">();"
    ctx << "}" << "else {"
    makeContinuation(clause)
    ctx << "}"
  }

  def extractProperties(
      call: EirFunctionCall
  )(implicit
      ctx: Context
  ): (List[EirDeclaration], List[String], List[EirType], List[String]) = {
    val arrayDecl = declarationsFor(call).toList
    val arrayName = arrayDecl.map(ctx.nameFor(_))
    val arrayType = arrayDecl.map(ctx.typeOf).map(CheckTypes.stripReference)
    (
      arrayDecl,
      arrayName,
      arrayType,
      arrayDecl.zip(arrayType).map { case (node, ty) =>
        ctx.typeFor(ty, Some(node))
      }
    )
  }

  def endBoundary(
      clause: Segmentation.Clause,
      call: EirFunctionCall,
      methodRef: String
  )(implicit ctx: Context): Unit = {
    val theirIdx = "__sender__"
    val (_, arrayName, _, arrayType) = extractProperties(call)
    val haloName = arrayName.map(x => s"__${x}_halo__")

    ctx << s"int $theirIdx;"
    haloName.zip(arrayType).foreach { case (name, ty) =>
      ctx << ty << name << ";"
    }
    unpack(Seq(theirIdx) ++ haloName)

    def moveAssign(f: String => String): Unit = {
      arrayName.map(f).zip(haloName).foreach { case (arr, halo) =>
        ctx << arr << "=" << "std::move(" << halo << ");"
      }
    }

    ctx << s"if (this->index() > $theirIdx) {"
    moveAssign(arrayAbove)
    ctx << "}" << "else {"
    moveAssign(arrayBelow)
    ctx << "}"

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
    ctx << "this->suspend<" << methodRef << ">();"
  }

  def endReduction(
      clause: Segmentation.Clause,
      call: EirFunctionCall
  )(implicit ctx: Context): Unit = {
    val target = declarationsFor(call).head
    unpack(Seq(ctx.nameFor(target)))
    makeContinuation(clause)
  }

  def beginGather(call: EirFunctionCall, methodRef: String)(implicit
      ctx: Context
  ): Unit = {
    implicit val visitor: (CodeGenerationContext, EirNode) => Unit =
      (ctx, x) => GenerateCpp.visit(x)(ctx)

    ctx << "auto __root__ = 0;"
    ctx << "auto __mine__ = this->index();"
    ctx << "auto __arguments__ = std::forward_as_tuple(__mine__, " << (call.args, ",") << ");"
    ctx << s"this->reduce<$methodRef>(__arguments__, CkReduction::set, __root__);"
    ctx << "if (__mine__ == __root__) {"
    ctx << s"this->suspend<$methodRef>();"
    ctx << "}" << "else {"
    ctx << "this->terminate();"
    ctx << "}"
  }

  def endGather(call: EirFunctionCall)(implicit
      ctx: Context
  ): Unit = {
    val (rawDecl, arrayName, rawType, arrayType) = extractProperties(call)
    val totalArray = arrayName.map(x => s"__${x}_complete__")
    val fragmentArray = arrayName.map(x => s"__${x}_fragment__")
    val src = "__src__"
    val set = "__set__"
    val idx = "__index__"
    rawDecl.zip(rawType).zip(totalArray).foreach { case ((decl, ty), name) =>
      val dimensions = ctx.info.dimensions(decl)
      ctx << s"auto $name = " << GenerateCpp.createArray(
        ctx.nameFor(ty, Some(decl)),
        dimensions,
        dimensions.size,
        initialize = false
      )(ctx.ctx) << ";"
    }
    ctx << s"std::shared_ptr<void> $src(std::move(" << payloadName << ".src));"
    ctx << s"auto *$set = (CkReduction::setElement *)$payloadName.data;"
    ctx << s"while ($set != nullptr) {"
    ctx << s"int $idx;"
    arrayType.zip(fragmentArray).foreach { case (ty, name) =>
      ctx << s"$ty $name;"
    }
    ctx << s"auto __arguments__ = std::forward_as_tuple($idx," << (fragmentArray, ",") << ");"
    ctx << s"hypercomm::unpacker p($src, (char *)$set->data);"
    ctx << "p | __arguments__;"
    ctx << s"CkAssert($set->dataSize == p.size());"
    totalArray.zip(fragmentArray).foreach { case (total, fragment) =>
      val offset = "__offset__"
      ctx << s"auto $offset = ($idx * $fragment->shape[0]) * $total->shape[1];"
      ctx << s"std::copy($fragment->begin(), $fragment->end(), $total->begin() + $offset);"
    }
    ctx << s"$set = $set->next();"
    ctx << "}"
    ctx << GenerateProxies.asyncFuture << s".set(hypercomm::pack(" << (totalArray, ",") << "));"
    ctx << "this->terminate();"
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
      case ClauseKind.Boundary  => beginBoundary(clause, call, methodRef)
      case ClauseKind.Reduction => beginReduction(call, methodRef)
      case ClauseKind.Gather    => beginGather(call, methodRef)
    }
    ctx << "}"

    ctx << "void" << payloadHeader(continuation) << "{"
    kind match {
      case ClauseKind.Boundary  => endBoundary(clause, call, methodRef)
      case ClauseKind.Reduction => endReduction(clause, call)
      case ClauseKind.Gather    => endGather(call)
    }
    ctx << "}"

    clause.successors.foreach(visit(_))
  }

  def continuation(
      construct: Segmentation.Construct
  )(implicit ctx: Context): Unit = {
    ctx << s"${nameFor(construct)}();"
  }

  def makeAccessor(array: String, counters: IndexedSeq[String]): String = {
    if (counters.size == 2) {
      s"(*$array)[(${counters.head}*$array->shape[1])+${counters.last}]"
    } else {
      ???
    }
  }

  def makeAt(declaration: EirDeclaration, expression: EirExpressionNode)(
      implicit ctx: Context
  ): Unit = {
    val arrayName = declaration.name
    val arrayTy = CheckTypes.stripReference(ctx.typeOf(declaration))
    val (ty, n) = arrayTy match {
      case EirTemplatedType(
            _,
            _,
            ty :: EirConstantFacade(EirIntegerLiteral(n)) :: _
          ) => (ctx.typeFor(ty), n)
    }
    val counters = (0 until n).map(x => s"__it_${x}__")
    ctx << s"inline $ty ${arrayName}_at(" << (counters.map(x =>
      s"int $x"
    ), ",") << ") {"
    val boundary = boundaryValue(expression)
    ((n - 1) to 1 by -1).foreach(i => {
      ctx << s"if (${counters(i)} < 0 || ${counters(i)} >= $arrayName->shape[$i]) {"
      boundary match {
        case ("pad", value) => ctx << s"return $value;"
      }
      ctx << "}"
    })
    def updateCounters(name: String, op: String): IndexedSeq[String] = {
      IndexedSeq(s"(__it_0__ $op $name->shape[0])") ++ counters.tail
    }
    ctx << Option.when(n >= 2)("else ") << "{"
    ctx << "if (__it_0__ < 0) {"
    ctx << s"return ${makeAccessor(arrayAbove(arrayName), updateCounters(arrayAbove(arrayName), "+"))};"
    ctx << "}" << s"else if (__it_0__ >= $arrayName->shape[0]) {"
    ctx << s"return ${makeAccessor(arrayBelow(arrayName), updateCounters(arrayName, "-"))};"
    ctx << "}" << "else {" << "{"
    ctx << s"return ${makeAccessor(arrayName, counters)};"
    ctx << "}" << "}" << "}" << "}"
  }

  def taskNameFor(fn: EirFunction): String = s"${fn.name}_task"

  def visit(fn: EirFunction)(implicit cgn: CodeGenerationContext): Unit = {
    val (res, graph) = Registry.instance[Stencil.Pass].apply(fn)
    val taskName = taskNameFor(fn)
    implicit val ctx: Context = Context(res, cgn, taskName)
    ctx << "class" << taskName << ":" << "public" << "hypercomm::tasking::task<" << taskName << ">" << "{"
    ctx << "public: // ;"
    val boundaries = visitFields(res.declarations)

    res.classifications
      .filter(_._2 == ClauseKind.Boundary)
      .keys
      .foreach(x => {
        ctx.counters :+= x
        ctx << s"int ${ctx.counterFor(x)};"
      })

    ctx.info.boundaries.foreach { case (x, y) => makeAt(x, y) }

    ctx << s"bool $hasAbove(void) const { return (this->index() > 0); }"
    ctx << s"bool $hasBelow(void) const {" << {
      "return ((this->index() + 1) < ergoline::workgroup_size());"
    } << "}"
    ctx << s"int $numNeighbors(void) const {" << {
      s"return (int)this->$hasAbove() + (int)this->$hasBelow();"
    } << "}"

    ctx << taskName << "(PUP::reconstruct) {}"
    ctx << payloadHeader(taskName) << "{"
    visitArguments(fn.functionArgs)
    makeBoundaries(boundaries)
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
