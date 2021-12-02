package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.Segmentation._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.{
  CppNode,
  RequestList,
  visitPatternDecl
}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable}
import edu.illinois.cs.ergoline.util.Errors

import scala.collection.mutable
import scala.util.Properties

object GenerateSdag {
  val sentinelType = "ergoline::sentinel_link"
  val speculatorType = "ergoline::speculator_link"
  val serverType = "hypercomm::state_server<hypercomm::microstack>"
  val stateType = s"typename $serverType::state_type"

  val generated: mutable.Map[EirMember, CodeGenerationContext] = mutable.Map()
  private var _current: Option[EirMember] = None

  type Continuation = (Construct, Option[String])
  type SymbolTable = List[(String, List[(String, EirResolvable[EirType])])]

  case class SdagGenerationContext(
      cgen: CodeGenerationContext,
      table: SymbolTable,
      contStack: mutable.Stack[Continuation] = new mutable.Stack(),
      callStack: mutable.Stack[Construct] = new mutable.Stack()
  ) {
    def enter(stkName: String): Unit = {
      windTable(cgen, table, stkName)
      cgen.pushProxySelf("self")
      cgen.pushSelf("self->impl_")
    }

    def leave(): Unit = {
      cgen.popSelf()
      cgen.popProxySelf()
      unwindTable(cgen, table)
    }

    def proxy: Option[EirProxy] = cgen.proxy

    def cloneWith(_table: SymbolTable): SdagGenerationContext = {
      SdagGenerationContext(this.cgen, _table, this.contStack, this.callStack)
    }

    def cloneWith(_cgen: CodeGenerationContext): SdagGenerationContext = {
      SdagGenerationContext(_cgen, this.table, this.contStack, this.callStack)
    }

    def cloneWith(
        _cgen: CodeGenerationContext,
        _table: SymbolTable
    ): SdagGenerationContext = {
      SdagGenerationContext(_cgen, _table, this.contStack, this.callStack)
    }

    def last: Option[(String, String, EirResolvable[EirType])] = {
      this.table.filter(_._2.nonEmpty).lastOption.flatMap {
        case (block, decls) =>
          decls.lastOption.map(decl => (block, decl._1, decl._2))
      }
    }

    def continuation: Option[Continuation] = contStack.headOption

    def pushContinuation(x: Continuation): Unit = {
      this.contStack.push(x)
    }

    def pushContinuation(o: Option[Continuation]): Unit = {
      o.foreach(x => this.contStack.push(x))
    }

    def popContinuation(o: Option[Continuation]): Unit = {
      if (o.nonEmpty) {
        assert(o == this.continuation)
        this.contStack.pop()
      }
    }
  }

  def current(implicit ctx: CodeGenerationContext): String = _current
    .map(ctx.nameFor(_))
    .map(s => s.substring(0, s.length - 1))
    .getOrElse("")

  def subcontext: CodeGenerationContext =
    _current.flatMap(generated.get).getOrElse(???)

  def prefixFor(construct: Construct, suffix: Option[String] = None)(implicit
      ctx: CodeGenerationContext
  ): String = {
    current + {
      construct match {
        case s: Loop => s"loop_${s.id}_${suffix
            .orElse(s.declarations.headOption.map(_ => "preheader"))
            .getOrElse("header")}"
        case s: MultiClause => s"await_${s.id}_lead_${suffix.getOrElse("in")}"
        case _              => s"block_${construct.id}"
      }
    } + "__"
  }

  def prefixFor(
      cont: Continuation
  )(implicit ctx: CodeGenerationContext): String = {
    prefixFor(cont._1, cont._2)(ctx)
  }

  def forwardState(
      ctx: SdagGenerationContext,
      stateName: String,
      stackName: String,
      forward: Boolean = true
  ): Unit = {
    ctx.cgen << stateType << "(" << stateName << s", ${if (forward) "std::move"
    else ""}(" << stackName << "))"
  }

  def continuation(
      ctx: SdagGenerationContext,
      to: Option[Continuation],
      arg: String,
      state: String,
      stack: String,
      append: Boolean = true,
      forward: Boolean = true
  ): Unit = {
    val cont = to.orElse(ctx.continuation)
    cont.map { case (x, y) => prefixFor(x, suffix = y)(ctx.cgen) } match {
      case Some(s) => ctx.cgen << s << "(" << arg << "," << forwardState(
          ctx,
          if (append) s"$state.first" else state,
          cont
            .map { case (x, _) =>
              val current = ctx.callStack.headOption
              val isSuccessor = current.zip(cont).exists { case (from, to) =>
                from.successors.contains(to._1)
              }
              val depth = {
                val tmp = current.map(_.depth - x.depth).getOrElse(0)
                if (
                  isSuccessor && current
                    .exists(x => x.encapsulate && x.declarations.nonEmpty)
                ) {
                  Math.max(tmp, 1)
                } else {
                  tmp
                }
              }
              var unwound = stack
              for (_ <- 0 until depth) {
                unwound += "->unwind()"
              }
              unwound
            }
            .getOrElse(stack),
          forward
        ) << ");"
      case None => {
        ctx.cgen << "// no continuation ;"
      }
    }
  }

  def continuation(
      ctx: SdagGenerationContext,
      to: Option[Construct],
      arg: String,
      state: String,
      stack: String
  ): Unit = continuation(ctx, to.map((_, None)), arg, state, stack)

  def continuation(
      ctx: SdagGenerationContext,
      to: List[Construct],
      arg: String,
      state: String,
      stack: String
  ): Unit = continuation(ctx, to.map((_, None)), arg, state, stack)

  def continuation(
      ctx: SdagGenerationContext,
      to: Iterable[Continuation],
      arg: String,
      state: String,
      stack: String
  ): Unit = {
    assert(to.isEmpty || to.size == 1)
    continuation(ctx, to.headOption, arg, state, stack)
  }

  private def functionHeader(
      name: String,
      proxy: Option[EirProxy],
      ctx: CodeGenerationContext,
      retTy: String = "void",
      attribute: Option[String] = None
  ): (String, String) = {
    val argName = "__arg__"
    val stateName = "__state__"
    ctx << attribute << "static" << retTy << name << "(" << "void*" << argName << "," << stateType << "&&" << stateName << ")" << "{"
    ctx << "auto*" << "self" << "=" << "(" << proxy.map(
      _.baseName
    ) << "*)hypercomm::access_context_();"
    (argName, stateName)
  }

  private def stringFromType(t: EirResolvable[EirType]): String = {
    t match {
      case EirPlaceholder(Some(n: CppNode), None) => n.s
      case _ => this._current
          .flatMap(this.generated.get)
          .map(_.typeFor(t, this._current))
          .getOrElse(???)
    }
  }

  def makeMicroStack(
      ctx: CodeGenerationContext,
      decls: List[EirResolvable[EirType]],
      stackName: String,
      prev: String,
      ptr: String = "unique",
      release: Boolean = true
  ): CodeGenerationContext = {
    val post = {
      if (release) {
        s"$prev.release()"
      } else {
        s"std::move($prev)"
      }
    }
    if (decls.isEmpty) {
      ctx << "auto" << stackName << "=" << post << ";"
    } else {
      ctx << s"std::${ptr}_ptr<hypercomm::microstack>" << stackName << "(" << "new"
      ctx << "hypercomm::typed_microstack<" << (decls.map(
        stringFromType
      ), ",") << ">"
      ctx << "(" << post << "," << (decls.map(_ =>
        "hypercomm::tags::no_init()"
      ), ",") << ")"
      ctx << ")" << ";"
    }
  }

  private def functionStack(
      ctx: SdagGenerationContext,
      decls: List[EirResolvable[EirType]],
      stateName: String,
      shared: Boolean = false,
      checkDepth: Boolean = true
  ): String = {
    val stkName = "__stk__"

    makeMicroStack(
      ctx.cgen,
      decls,
      stkName, {
        s"$stateName.second"
      }, {
        if (shared) "shared" else "unique"
      }
    )

    if (checkDepth) {
      val depth = ctx.callStack.headOption.map(_.depth).getOrElse(???)
      if (depth > 0) {
        ctx.cgen << s"CkAssert($stkName && $stkName->depth == ${depth});"
      } else {
        ctx.cgen << s"CkAssert(!$stkName);"
      }
    }

    stkName
  }

  def collectDeclarations(
      nodes: Iterable[EirNode]
  ): List[(String, EirResolvable[EirType])] = {
    nodes
      .collect {
        case x: EirDeclaration      => Seq((x.name, x.declaredType))
        case x: EirFunctionArgument => Seq((x.name, x.declaredType))
        case x: EirMultiDeclaration =>
          x.children.map(c => (c.name, c.declaredType))
      }
      .flatten
      .toList
  }

  def offsetFor(blockName: String, declName: String): String = {
    s"${blockName.substring(0, blockName.length - 1)}${declName}_offset__"
  }

  def derefVariable(
      ctx: CodeGenerationContext,
      blockName: String,
      decl: (String, EirResolvable[EirType]),
      stkName: String
  ): (String, String) = {
    val ty = stringFromType(decl._2)
    (s"$stkName->at<$ty>(${offsetFor(blockName, decl._1)})", ty)
  }

  def windTable(
      ctx: CodeGenerationContext,
      table: SymbolTable,
      stkName: String
  ): Unit = {
    table.foreach { case (blockName, decls) =>
      decls.foreach { decl =>
        ctx.putReplacement(
          decl._1,
          derefVariable(ctx, blockName, decl, stkName)._1
        )
      }
    }
  }

  def unwindTable(ctx: CodeGenerationContext, table: SymbolTable): Unit = {
    table.foreach { case (_, decls) =>
      decls.foreach { decl => ctx.popReplacement(decl._1) }
    }
  }

  def declareOffsets(
      ctx: SdagGenerationContext,
      prev: Option[(String, String, EirResolvable[EirType])],
      blockName: String,
      decls: List[(String, EirResolvable[EirType])]
  ): Unit = {
    decls.headOption.foreach { case (declName, _) =>
      ctx.cgen << s"static constexpr auto ${offsetFor(blockName, declName)} = ${prev
        .map { case (block, name, _) => s"${offsetFor(block, name)} + 1" }
        .getOrElse("0")};"
    }
    if (decls.nonEmpty) decls.tail.indices.foreach { i =>
      ctx.cgen << s"static constexpr auto ${offsetFor(
        blockName,
        decls(i + 1)._1
      )} = ${offsetFor(blockName, decls(i)._1)} + 1;"
    }
  }

  def visitDeclaration(ctx: SdagGenerationContext, d: EirDeclaration)(
      blockName: String,
      stkName: String
  ): Unit = {
    val (ref, ty) =
      derefVariable(ctx.cgen, blockName, (d.name, d.declaredType), stkName)
    ctx.cgen << "new (&(" << ref << ")) " << ty << "(" << d.initialValue
      .foreach(GenerateCpp.visit(_)(ctx.cgen)) << ");"
  }

  def visit(
      _ctx: SdagGenerationContext,
      block: SerialBlock
  ): SymbolTable = {
    val decls = collectDeclarations(block.slst)
    val blockName = prefixFor(block)(_ctx.cgen)
    val blockTable = _ctx.table :+ (blockName, decls)
    val sctx = _ctx.cloneWith(subcontext, blockTable)
    declareOffsets(sctx, _ctx.last, blockName, decls)
    val (argName, stateName) = functionHeader(
      blockName,
      _ctx.proxy,
      sctx.cgen,
      attribute = Option.when(block.threaded)("/* threaded */ ")
    )
    val stkName = functionStack(sctx, decls.map(_._2), stateName)
    sctx.cgen << "auto*" << "__server__" << "=" << s"(std::shared_ptr<$serverType>*)$argName;"
    sctx.enter(stkName)
    block.slst.foreach {
      case d: EirDeclaration => visitDeclaration(sctx, d)(blockName, stkName)
      case n: EirNode        => sctx.cgen << GenerateCpp.visit(n)(sctx.cgen) << ";"
    }
    continuation(sctx, block.successors, argName, stateName, stkName)
    sctx.leave()
    sctx.cgen << "}"
    blockTable
  }

  def visit(
      ctx: SdagGenerationContext,
      loop: Loop
  ): SymbolTable = {
    val overlap = loop.node.hasAnnotation("overlap")
    val cont: Continuation = {
      if (overlap) {
        (loop, Some("sentinel"))
      } else {
        latchName(ctx, loop)
      }
    }
    // TODO ( add support for do-while loops )
    val (headerTable, argName) = visitLoopHeader(
      ctx.cloneWith(subcontext),
      loop,
      ctx.proxy
    )
    val sctx = ctx.cloneWith(subcontext, headerTable)
    if (!overlap) {
      visitLoopLatch(sctx, loop, ctx.proxy, argName)
    }
    ctx.pushContinuation(cont)
    loop.body.map(visit(sctx, _))
    ctx.popContinuation(Some(cont))
    ctx.table
  }

  val argType =
    "std::tuple<void*, std::size_t, std::shared_ptr<hypercomm::microstack>>"

  def makeRequest(
      clause: Clause,
      when: EirSdagWhen,
      stateName: String,
      stackName: String,
      argName: String,
      continuation: Continuation
  )(
      ctx: SdagGenerationContext
  ): (RequestList, String) = {
    val list = GenerateCpp.generateRequestList(when)(ctx.cgen)
    val ty = GenerateCpp.joinRequestTypes(list)
    val com = "__com__"
    val set = "__set__"
    val setTy = s"${set.substring(0, set.length - 1)}type__"

    ctx.cgen << "using" << setTy << "=" << ty << ";"
    ctx.cgen << "auto" << com << "=" << s"ergoline::make_component<$setTy, $argType>(*" << ctx.cgen.currentProxySelf << ","
    ctx.cgen << wrapperFor(
      prefixFor(continuation)(ctx.cgen)
    ) << "," << "std::make_tuple(" << argName << s", $stateName.first, " << stackName << ")"
    ctx.cgen << ");"

    GenerateCpp.generateRequest(when, list, com)(ctx.cgen)

    ctx.cgen << ctx.cgen.currentProxySelf << "->activate_component(" << com << ");"

    ctx.cgen << "return" << com << ";"

    (list, ty)
  }

  def makeWrapper(
      blockName: String,
      name: Continuation,
      arg: String,
      ty: String,
      decls: List[(String, EirResolvable[EirType])],
      cont: Option[Continuation]
  )(
      ctx: SdagGenerationContext,
      when: EirSdagWhen
  ): Unit = {
    val set = "__set__"
    val stk = "__stk__"
    val from = prefixFor(name)(ctx.cgen)

    ctx.cgen << "static" << "void" << wrapperFor(
      from
    ) << "(" << ty << "&" << set << "," << argType << "&&" << arg << ")" << "{"

    makeMicroStack(
      ctx.cgen,
      decls.map(_._2),
      stk,
      s"std::get<2>($arg)",
      release = false
    )

    when.patterns.zipWithIndex.foreach { case ((_, patterns), i) =>
      val name = s"__value${i}__"

      ctx.cgen << "auto" << name << "=" << s"std::move(std::get<$i>($set));"

      ctx.cgen << visitPatternDecl(
        ctx.cgen,
        patterns,
        name + "->value()",
        format = (
            ctx: CodeGenerationContext,
            name: String,
            _ty: Option[String],
            rhs: GenerateCpp.LazyCodeBlock
        ) => {
          val ty = _ty.getOrElse(Errors.missingType(null))
          ctx << s"new (&$stk->at<$ty>(${offsetFor(blockName, name)})) $ty(" << rhs() << ");"
        }
      ).split(Properties.lineSeparator)
    }

    continuation(
      ctx,
      cont,
      s"std::get<0>($arg)",
      s"std::get<1>($arg)",
      stk,
      append = false
    )

    ctx.cgen << "}"
  }

  def wrapperFor(blockName: String): String = {
    blockName.substring(0, blockName.length - 1) + "wrapper__"
  }

  def visit(
      _ctx: SdagGenerationContext,
      clause: Clause
  ): SymbolTable = {
    val decls = collectDeclarations(clause.node match {
      case x: EirSdagWhen => x.declarations
      case _              => ???
    })
    val name = prefixFor(clause)(_ctx.cgen)
    val table = _ctx.table :+ (name, decls)
    val sctx = _ctx.cloneWith(subcontext)
    declareOffsets(sctx, _ctx.last, name, decls)
    val (argName, stateName) = functionHeader(
      name,
      _ctx.cgen.proxy,
      sctx.cgen,
      "hypercomm::component_id_t"
    )
    val stkName =
      functionStack(sctx, Nil, stateName, shared = true, checkDepth = false)
    val wrappedBlock: Continuation = (clause.body.getOrElse(clause), None)
    sctx.enter(stkName)
    val (_, reqTy) = clause.node match {
      case x: EirSdagWhen =>
        makeRequest(clause, x, stateName, stkName, argName, wrappedBlock)(sctx)
    }
    sctx.leave()
    sctx.cgen << "}"

    val succ = {
      clause.successors.headOption.map((_, None)).orElse(sctx.continuation)
    }

    visitAllWithContinuation(sctx.cloneWith(table), succ, clause.body)

    val cont = clause.body.map(_ => wrappedBlock).orElse(succ)
    clause.node match {
      case x: EirSdagWhen =>
        makeWrapper(name, wrappedBlock, argName, reqTy, decls, cont)(sctx, x)
    }

    _ctx.table
  }

  def visitLoopHeader(
      _ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy]
  ): (SymbolTable, String) = {
    val _decls = loop.declarations
    val decls = collectDeclarations(_decls)
    // can be either pre/header
    val blockName = prefixFor(loop)(_ctx.cgen)
    val blockTable = _ctx.table :+ (blockName, decls)
    val ctx = _ctx.cloneWith(blockTable)
    declareOffsets(ctx, _ctx.last, blockName, decls)
    val (argName, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx, decls.map(_._2), stateName)
    val overlap = loop.node.hasAnnotation("overlap")
    val unwind = decls.nonEmpty
    val cont = {
      if (loop.successors.isEmpty && overlap) {
        ctx.continuation
      } else {
        None
      }
    }
    ctx.pushContinuation(cont)
    if (decls.isEmpty) {
      visitLoopHeaderLast(ctx, loop, unwind)(
        stateName,
        stkName,
        argName
      )
    } else {
      ctx.enter(stkName)
      val headerName = (loop, Some("header"))
      _decls.flatMap {
        case d: EirDeclaration      => List(d)
        case d: EirMultiDeclaration => d.children
      } foreach { d => visitDeclaration(ctx, d)(blockName, stkName) }
      if (overlap) {
        val linkName = "__link__"
        ctx.cgen << "auto*" << linkName << "=" << "new" << sentinelType << "(" << asLink(
          argName
        ) << ")" << ";"
        continuation(ctx, Some(headerName), linkName, stateName, stkName)
      } else {
        continuation(ctx, Some(headerName), argName, stateName, stkName)
      }
      ctx.cgen << "}"
      if (overlap) {
        visitLoopSentinel(ctx, loop, proxy, unwind)
      }
      ctx.leave()
      visitLoopHeaderInit(ctx, loop, proxy, prefixFor(headerName)(ctx.cgen))
    }
    ctx.popContinuation(cont)
    (blockTable, argName)
  }

  def visitLoopSentinel(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      unwind: Boolean
  ): Unit = {
    val sentinelName = prefixFor(loop, suffix = Some("sentinel"))(ctx.cgen)
    val (argName, stateName) = functionHeader(sentinelName, proxy, ctx.cgen)
    val stackName = s"$stateName.second"
    ctx.cgen << s"(${asSentinel(argName)})->consume();"
    ctx.cgen << s"if ((${asSentinel(argName)})->complete())" << "{"
    visitLoopContinuation(ctx, loop)(
      stateName,
      stackName,
      argName,
      unwind,
      overlap = true
    )
    ctx.cgen << "}" << "}"
  }

  def visitLoopHeaderInit(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      blockName: String
  ): Unit = {
    val (argName, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx, Nil, stateName)
    visitLoopHeaderLast(ctx, loop, unwind = true)(stateName, stkName, argName)
  }

  def latchName(ctx: SdagGenerationContext, loop: Loop): Continuation = {
    (loop, Some("latch"))
  }

  def asLink(s: String): String = {
    s"(ergoline::link *)$s"
  }

  def asSentinel(s: String): String = {
    s"($sentinelType *)$s"
  }

  def unlink(s: String): String = {
    s"(${asLink(s)})->prev"
  }

  def visitLoopHeaderLast(
      ctx: SdagGenerationContext,
      loop: Loop,
      unwind: Boolean
  )(
      stateName: String,
      stackName: String,
      argName: String
  ): Unit = {
    val overlap = loop.node.hasAnnotation("overlap")
    ctx.enter(stackName)
    ctx.cgen << {
      if (overlap) {
        "while"
      } else {
        "if"
      }
    } << "("
    loop.node match {
      case x: EirForLoop => x.header match {
          case EirCStyleHeader(_, test, _) => GenerateCpp.visit(test)(ctx.cgen)
        }
    }
    ctx.cgen << ")" << "{"
    if (overlap) {
      ctx.cgen << s"(${asSentinel(argName)})->produce();"
      continuation(ctx, loop.body, argName, stateName, s"$stackName->clone()")
      visitLatchImplementation(ctx, loop)
    } else {
      continuation(ctx, loop.body, argName, stateName, stackName)
    }
    ctx.cgen << "}"
    if (overlap) {
      ctx.cgen << s"if ((${asSentinel(argName)})->deactivate())"
    } else {
      ctx.cgen << "else"
    }
    ctx.cgen << "{"
    visitLoopContinuation(ctx, loop)(
      stateName,
      stackName,
      argName,
      unwind,
      overlap
    )
    ctx.cgen << "}" << "}"
    ctx.leave()
  }

  def deleteLink(
      ctx: SdagGenerationContext,
      ty: String,
      name: String
  ): String = {
    val linkName = s"__link${name.substring(1)}"
    ctx.cgen << "auto*" << linkName << "=" << unlink(name) << ";"
    ctx.cgen << "delete" << s"($ty*)" << name << ";"
    linkName
  }

  def visitLoopContinuation(ctx: SdagGenerationContext, loop: Loop)(
      stateName: String,
      stackName: String,
      argName: String,
      unwind: Boolean,
      overlap: Boolean
  ): Unit = {
    val arg = {
      if (overlap) {
        deleteLink(ctx, sentinelType, argName)
      } else {
        argName
      }
    }

    continuation(
      ctx,
      loop.successors,
      arg,
      stateName,
      stackName /* {
        if (unwind && loop.successors.nonEmpty) s"$stackName->unwind()"
        else stackName
      } */
    )
  }

  def visitLatchImplementation(
      ctx: SdagGenerationContext,
      loop: Loop
  ): Unit = {
    loop.node match {
      case x: EirForLoop => x.header match {
          case y: EirCStyleHeader =>
            GenerateCpp.visit(y.increment)(ctx.cgen)
            ctx.cgen << ";"
          case _ => ???
        }
      case _ => ???
    }
  }

  def visitLoopLatch(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      argName: String
  ): Unit = {
    val blockName = prefixFor(latchName(ctx, loop))(ctx.cgen)
    val (_, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx, Nil, stateName)
    ctx.enter(stkName)
    visitLatchImplementation(ctx, loop)
    continuation(
      ctx,
      Some((loop, Some("header"))),
      argName,
      stateName,
      stkName
    )
    ctx.leave()
    ctx.cgen << "}"
  }

  val comIdType = "hypercomm::component_id_t"

  def makeLeadIn(
      _ctx: SdagGenerationContext,
      multiClause: MultiClause
  ): SdagGenerationContext = {
    val all = multiClause.node.waitAll
    val blockName = prefixFor(multiClause)(_ctx.cgen)
    val ctx = _ctx.cloneWith(subcontext)
    val (prevName, stateName) = functionHeader(blockName, _ctx.proxy, ctx.cgen)
    val stkName = functionStack(ctx, Nil, stateName, shared = true)
    val members = multiClause.members
    val linkName = "__link__"

    assert(members.nonEmpty)

    ctx.cgen << "auto*" << linkName << "=" << "new"
    if (all) {
      ctx.cgen << sentinelType << "(" << asLink(prevName) << ");"
    } else {
      ctx.cgen << speculatorType << "(" << asLink(
        prevName
      ) << "," << members.size.toString << ");"
    }

    members.zipWithIndex.foreach { case (x, i) =>
      val last = i == (members.size - 1)

      if (all) {
        ctx.cgen << linkName << "->produce();"

        if (last) {
          ctx.cgen << linkName << "->deactivate();"
        }
      } else {
        ctx.cgen << linkName << s"->ids[$i]" << "="
      }

      continuation(
        ctx,
        Some((x, None)),
        linkName,
        stateName,
        stkName,
        forward = last
      )
    }

    ctx.cgen << "}"

    ctx
  }

  def makeLeadOut(
      ctx: SdagGenerationContext,
      proxy: Option[EirProxy],
      multiClause: MultiClause,
      leadOut: String
  ): Unit = {
    val (argName, stateName) = functionHeader(leadOut, proxy, ctx.cgen)
    val stkName = functionStack(ctx, Nil, stateName, shared = true)
    val numMembers = multiClause.members.length
    val all = multiClause.node.waitAll

    continuation(
      ctx,
      multiClause.successors,
      if (all) {
        ctx.cgen << s"(${asSentinel(argName)})->consume();"
        ctx.cgen << "if" << s"((${asSentinel(argName)})->complete())" << "{"
        deleteLink(ctx, sentinelType, argName)
      } else {
        val ids = "__ids__"
        ctx.cgen << s"auto& $ids = (($speculatorType*)$argName)->ids;"
        for (i <- 0 until numMembers) {
          ctx.cgen << s"if (self->is_idle($ids[$i]))" << s"self->invalidate_component($ids[$i]);"
        }
        deleteLink(ctx, speculatorType, argName)
      },
      stateName,
      stkName
    )

    ctx.cgen << Option.when(all)("}") << "}"
  }

  def visitAllWithContinuation(
      ctx: SdagGenerationContext,
      cont: Option[Continuation],
      seq: Iterable[Construct]
  ): Unit = {
    // consumes a continuation if one is not provided
    ctx.pushContinuation(cont)
    seq.foreach(visit(ctx, _))
    ctx.popContinuation(cont)
  }

  def visit(
      _ctx: SdagGenerationContext,
      multiClause: MultiClause
  ): SymbolTable = {
    val ctx = makeLeadIn(_ctx, multiClause)
    val leadOut = (multiClause, Some("out"))
    visitAllWithContinuation(ctx, Some(leadOut), multiClause.members)
    makeLeadOut(ctx, _ctx.proxy, multiClause, prefixFor(leadOut)(ctx.cgen))
    _ctx.table
  }

  def visit(
      _ctx: SdagGenerationContext,
      divergence: Divergence
  ): SymbolTable = {
    val ctx = _ctx.cloneWith(subcontext)
    val cont = divergence.successors.headOption
    assert(divergence.successors.size <= 1)
    val blockName = prefixFor(divergence)(ctx.cgen)
    val (argName, stateName) = functionHeader(blockName, _ctx.proxy, ctx.cgen)
    val stkName = functionStack(ctx, Nil, stateName, shared = true)
    ctx.enter(stkName)

    val members = divergence.members
    divergence.node match {
      case x: EirIfElse =>
        val ifTrue = members.headOption
        val ifFalse = members.lastOption.filter(_ => members.size == 2)
        ctx.cgen << "if" << "(" << GenerateCpp.visit(x.test)(
          ctx.cgen
        ) << ")" << "{"
        continuation(ctx, ifTrue, argName, stateName, stkName)
        ctx.cgen << "}" << "else" << "{"
        continuation(ctx, ifFalse.orElse(cont), argName, stateName, stkName)
        ctx.cgen << "}"
      case _ => ???
    }

    ctx.cgen << "}"
    ctx.leave()

    visitAllWithContinuation(ctx, cont.map((_, None)), members)

    ctx.table
  }

  def visit(
      ctx: SdagGenerationContext,
      construct: Construct
  ): SymbolTable = {
    ctx.callStack.push(construct)

    val next = construct match {
      case x: SerialBlock => visit(ctx, x)
      case x: Loop        => visit(ctx, x)
      case x: Clause      => visit(ctx, x)
      case x: Divergence  => visit(ctx, x)
      case x: MultiClause => visit(ctx, x)
      case _              => ???
    }

    if (construct.successors.size <= 1) {
      val last = construct.successors.foldLeft(next)((next, nextConstruct) => {
        visit(ctx.cloneWith(next), nextConstruct)
      })

      assert(construct == ctx.callStack.pop())

      last
    } else {
      ???
    }
  }

  def visit(member: EirMember, fn: EirFunction)(implicit
      ctx: CodeGenerationContext
  ): Boolean = {
    assert(member.hasAnnotation("threaded"))
    val res = Registry.instance[Pass].apply(fn)
    res.nonEmpty && {
      ctx << "{"
      _current = Some(member)
      generated.put(member, ctx.makeSubContext())
      val decls = collectDeclarations(fn.functionArgs)
      val stkName = "__stk__"
      val table: SymbolTable = {
        if (decls.isEmpty) {
          ctx << s"hypercomm::microstack*" << stkName << "=" << "nullptr;"
          Nil
        } else {
          val blkName = ctx.nameFor(member)
          val table = List((blkName, decls))
          declareOffsets(
            SdagGenerationContext(subcontext, table),
            None,
            blkName,
            decls
          )
          makeMicroStack(
            ctx,
            decls.map(_._2),
            stkName,
            "nullptr",
            release = false
          )
          ctx << ";"
          decls.foreach(decl => {
            val ref = derefVariable(ctx, blkName, decl, stkName)
            ctx << s"new (&${ref._1}) ${ref._2}(" << decl._1 << ");"
          })
          table
        }
      }
      res.foldLeft(table)((table, construct) => {
        val next = visit(SdagGenerationContext(ctx, table), construct)
        ctx << prefixFor(construct)(
          ctx
        ) << s"(nullptr, std::make_pair(0, std::move($stkName)));"
        next
      })
      _current = None
      ctx << "}"
      true
    }
  }
}
