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
  val serverType = "hypercomm::state_server<hypercomm::microstack>"
  val stateType = s"typename $serverType::state_type"

  val generated: mutable.Map[EirMember, CodeGenerationContext] = mutable.Map()
  private var _current: Option[EirMember] = None

  type SymbolTable = List[(String, List[(String, EirResolvable[EirType])])]

  case class SdagGenerationContext(
      cgen: CodeGenerationContext,
      table: SymbolTable,
      var continuation: Option[Any] = None,
      stack: mutable.Stack[Construct] = new mutable.Stack()
  ) {
    def deeper: Boolean = {
      val n = this.stack.length
      (n == 0) || {
        (n == 1) && (this.stack(0).depth > 0)
      } || {
        this.stack(0).depth > this.stack(1).depth
      }
    }

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

    def cloneWith(cont: Option[Any]): SdagGenerationContext = {
      SdagGenerationContext(cgen, table, cont, stack)
    }

    def cloneWith(cgen: CodeGenerationContext): SdagGenerationContext = {
      SdagGenerationContext(cgen, this.table, this.continuation, stack)
    }

    def cloneWith(table: SymbolTable): SdagGenerationContext = {
      SdagGenerationContext(this.cgen, table, this.continuation, stack)
    }

    def cloneWith(
        cgen: CodeGenerationContext,
        table: SymbolTable
    ): SdagGenerationContext = {
      SdagGenerationContext(cgen, table, this.continuation, stack)
    }

    def last: Option[(String, String, EirResolvable[EirType])] = {
      this.table.filter(_._2.nonEmpty).lastOption.flatMap {
        case (block, decls) =>
          decls.lastOption.map(decl => (block, decl._1, decl._2))
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
      to: Option[Any],
      arg: String,
      state: String,
      stack: String,
      append: Boolean = true,
      forward: Boolean = true
  ): Unit = {
    to.orElse(ctx.continuation).map {
      case x: Construct => prefixFor(x)(ctx.cgen)
      case x: String    => x
      case _            => ???
    } match {
      case Some(s) => ctx.cgen << s << "(" << arg << "," << forwardState(
          ctx,
          if (append) s"$state.first" else state,
          if (to.isEmpty && ctx.deeper) s"$stack->unwind()" else stack,
          forward
        ) << ");"
      case None => ctx.cgen << "// no continuation ;"
    }
  }

  def continuation(
      ctx: SdagGenerationContext,
      to: Seq[Construct],
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
      prev: String,
      decls: List[EirResolvable[EirType]]
  ): CodeGenerationContext = {
    if (decls.isEmpty) {
      ctx << prev
    } else {
      ctx << "new hypercomm::typed_microstack<" << (decls.map(
        stringFromType
      ), ",") << ">("
      ctx << prev << ","
      ctx << (decls.map(_ => "hypercomm::tags::no_init()"), ",") << ")"
    }
  }

  private def functionStack(
      ctx: CodeGenerationContext,
      decls: List[EirResolvable[EirType]],
      stateName: String,
      shared: Boolean = false
  ): String = {
    val stkName = "__stk__"

    val makeStack = (release: Boolean) => {
      if (decls.isEmpty) {
        ctx << s"$stateName.second${if (release) ".release()" else ""}"
      } else {
        makeMicroStack(ctx, s"$stateName.second.release()", decls)
      }
    }

    if (shared) {
      ctx << "auto" << stkName << "=" << s"std::shared_ptr<hypercomm::microstack>(" << makeStack(
        true
      ) << ");"
    } else {
      ctx << {
        if (decls.isEmpty) {
          "auto&"
        } else {
          "auto*"
        }
      } << stkName << "=" << makeStack(false) << ";"
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
    val (argName, stateName) = functionHeader(blockName, _ctx.proxy, sctx.cgen, attribute = Option.when(block.threaded)("/* threaded */ "))
    val stkName = functionStack(sctx.cgen, decls.map(_._2), stateName)
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
    // TODO ( add support for do-while loops )
    val (headerTable, argName) = visitLoopHeader(
      ctx.cloneWith(subcontext),
      loop,
      ctx.proxy
    )
    val sctx = ctx.cloneWith(subcontext, headerTable)
    sctx.continuation =
      Some(prefixFor(loop, suffix = Some("latch"))(subcontext))
    visitLoopLatch(sctx, loop, ctx.proxy, argName)
    loop.body.map(visit(sctx, _))
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
      wrapperName: String
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
      wrapperName
    ) << "," << "std::make_tuple(" << argName << s", $stateName.first, " << stackName << ")"
    ctx.cgen << ");"

    GenerateCpp.generateRequest(when, list, com)(ctx.cgen)

    ctx.cgen << ctx.cgen.currentProxySelf << "->activate_component(" << com << ");"

    ctx.cgen << "return" << com << ";"

    (list, ty)
  }

  def makeWrapper(
      blockName: String,
      name: String,
      arg: String,
      ty: String,
      decls: List[(String, EirResolvable[EirType])],
      cont: Option[Any]
  )(
      ctx: SdagGenerationContext,
      when: EirSdagWhen
  ): Unit = {
    val set = "__set__"
    val stk = "__stk__"

    ctx.cgen << "static" << "void" << wrapperFor(
      name
    ) << "(" << ty << "&" << set << "," << argType << "&&" << arg << ")" << "{"

    ctx.cgen << {
      if (decls.isEmpty) "auto&" else "auto*"
    } << stk << "=" << makeMicroStack(
      ctx.cgen,
      s"std::get<2>($arg)",
      decls.map(_._2)
    ) << ";"

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
    val stkName = functionStack(sctx.cgen, Nil, stateName, shared = true)
    val wrappedBlock = clause.body.map(prefixFor(_)(sctx.cgen)).getOrElse(name)
    sctx.enter(stkName)
    val (_, reqTy) = clause.node match {
      case x: EirSdagWhen =>
        makeRequest(clause, x, stateName, stkName, argName, wrappedBlock)(sctx)
    }
    sctx.leave()
    sctx.cgen << "}"

    clause.body.foreach(visit(_ctx.cloneWith(table), _))

    val cont = clause.body.map(_ => wrappedBlock).orElse(clause.successors.headOption)
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
    val stkName = functionStack(ctx.cgen, decls.map(_._2), stateName)
    if (decls.isEmpty) {
      visitLoopHeaderLast(ctx, loop, decls.nonEmpty)(
        stateName,
        stkName,
        argName
      )
    } else {
      val headerName = prefixFor(loop, suffix = Some("header"))(_ctx.cgen)
      _decls.flatMap {
        case d: EirDeclaration      => List(d)
        case d: EirMultiDeclaration => d.children
      } foreach { d => visitDeclaration(ctx, d)(blockName, stkName) }
      continuation(ctx, Some(headerName), argName, stateName, stkName)
      ctx.cgen << "}"
      visitLoopHeaderInit(ctx, loop, proxy, headerName)
    }
    (blockTable, argName)
  }

  def visitLoopHeaderInit(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      blockName: String
  ): Unit = {
    val (argName, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx.cgen, Nil, stateName)
    visitLoopHeaderLast(ctx, loop, unwind = true)(stateName, stkName, argName)
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
    ctx.enter(stackName)
    ctx.cgen << "if" << "("
    loop.node match {
      case x: EirForLoop => x.header match {
          case EirCStyleHeader(_, test, _) => GenerateCpp.visit(test)(ctx.cgen)
        }
    }
    ctx.cgen << ")" << "{"
    continuation(ctx, loop.body, argName, stateName, stackName)
    ctx.cgen << "}" << "else" << "{"
    continuation(
      ctx,
      loop.successors,
      argName,
      stateName, {
        if (unwind && loop.successors.nonEmpty) s"$stackName->unwind()"
        else stackName
      }
    )
    ctx.cgen << "}" << "}"
    ctx.leave()
  }

  def visitLoopLatch(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      argName: String
  ): Unit = {
    val blockName = prefixFor(loop, suffix = Some("latch"))(ctx.cgen)
    val (_, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx.cgen, Nil, stateName)
    ctx.enter(stkName)
    loop.node match {
      case x: EirForLoop => x.header match {
          case y: EirCStyleHeader =>
            GenerateCpp.visit(y.increment)(ctx.cgen)
            ctx.cgen << ";"
          case _ => ???
        }
      case _ => ???
    }
    continuation(
      ctx,
      Some(prefixFor(loop, suffix = Some("header"))(ctx.cgen)),
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
  ): (SdagGenerationContext, List[String]) = {
    val all = multiClause.node.waitAll
    val decls =
      if (all) List(("__recvd__", globals.integerType))
      else List(
        (
          "__ids__",
          EirPlaceholder[EirType](Some(CppNode(s"$comIdType*")), None)
        )
      )
    val blockName = prefixFor(multiClause)(_ctx.cgen)
    val blockTable = _ctx.table :+ (blockName, decls)
    val ctx = _ctx.cloneWith(subcontext, blockTable)
    declareOffsets(ctx, _ctx.last, blockName, decls)
    val (argName, stateName) = functionHeader(blockName, _ctx.proxy, ctx.cgen)
    val stkName =
      functionStack(ctx.cgen, decls.map(_._2), stateName, shared = true)
    val members = multiClause.members
    val refs = decls.map { case (ref, ty) =>
      derefVariable(
        ctx.cgen,
        blockName,
        (ref, ty),
        stkName
      )._1
    }
    refs.foreach(ref =>
      if (all) {
        ctx.cgen << ref << "=0;"
      } else {
        ctx.cgen << ref << "=" << "new" << comIdType << "[" << members.size.toString << "];"
      }
    )
    members.zipWithIndex.foreach { case (x, i) =>
      if (!all) {
        ctx.cgen << refs.headOption << s"[$i]" << "="
      }
      continuation(
        ctx,
        Some(x),
        argName,
        stateName,
        stkName,
        forward = i == (members.size - 1)
      )
    }
    ctx.cgen << "}"
    (ctx, refs)
  }

  def makeLeadOut(
      ctx: SdagGenerationContext,
      proxy: Option[EirProxy],
      multiClause: MultiClause,
      leadOut: String,
      refs: List[String]
  ): Unit = {
    val (argName, stateName) = functionHeader(leadOut, proxy, ctx.cgen)
    val stkName = functionStack(ctx.cgen, Nil, stateName, shared = true)
    val numMembers = multiClause.members.length

    if (multiClause.node.waitAll) {
      assert(refs.length == 1)
      ctx.cgen << refs.head << "+=1;"
      ctx.cgen << "if" << "(" << refs.head << "==" << numMembers.toString << ")" << "{"
      continuation(ctx, multiClause.successors, argName, stateName, stkName)
      ctx.cgen << "}"
    } else {
      assert(refs.length == 1)
      val ids = "__ids__"
      ctx.cgen << s"auto& $ids = " << refs.head << ";"
      for (i <- 0 until numMembers) {
        ctx.cgen << s"if (self->is_idle($ids[$i]))" << s"self->invalidate_component($ids[$i]);"
      }
      ctx.cgen << s"delete $ids;"
      continuation(ctx, multiClause.successors, argName, stateName, stkName)
    }

    ctx.cgen << "}"
  }

  def visit(
      _ctx: SdagGenerationContext,
      multiClause: MultiClause
  ): SymbolTable = {
    val (ctx, refs) = makeLeadIn(_ctx, multiClause)
    val leadOut = prefixFor(multiClause, suffix = Some("out"))(ctx.cgen)
    multiClause.members.foreach(visit(ctx.cloneWith(Some(leadOut)), _))
    makeLeadOut(ctx, _ctx.proxy, multiClause, leadOut, refs)
    _ctx.table
  }

  def visit(
      ctx: SdagGenerationContext,
      construct: Construct
  ): SymbolTable = {
    ctx.stack.push(construct)

    val next = construct match {
      case x: SerialBlock => visit(ctx, x)
      case x: Loop        => visit(ctx, x)
      case x: Clause      => visit(ctx, x)
      case _: Divergence  => ???
      case x: MultiClause => visit(ctx, x)
      case _              => ???
    }

    if (construct.successors.size <= 1) {
      val last = construct.successors.foldLeft(next)((next, nextConstruct) => {
        visit(ctx.cloneWith(next), nextConstruct)
      })

      assert(construct == ctx.stack.pop())

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
      ctx << s"hypercomm::microstack*" << stkName << "="
      val table: SymbolTable = {
        if (decls.isEmpty) {
          ctx << "nullptr;"
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
          makeMicroStack(ctx, "nullptr", decls.map(_._2))
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
        ) << s"(nullptr, std::make_pair(0, $stkName));"
        next
      })
      _current = None
      ctx << "}"
      true
    }
  }
}
