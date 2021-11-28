package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.Segmentation._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{
  EirCStyleHeader,
  EirDeclaration,
  EirForAllHeader,
  EirForLoop,
  EirFunction,
  EirMember,
  EirMultiDeclaration,
  EirNode,
  EirSdagWhen
}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.EirResolvable

import scala.collection.mutable

object GenerateSdag {
  val serverType = "hypercomm::state_server<hypercomm::microstack>"
  val stateType = s"typename $serverType::state_type"

  val generated: mutable.Map[EirMember, CodeGenerationContext] = mutable.Map()
  private var _current: Option[EirMember] = None

  type SymbolTable = List[(String, List[(String, EirResolvable[EirType])])]

  case class SdagGenerationContext(
      cgen: CodeGenerationContext,
      table: SymbolTable,
      continuation: Option[Any] = None
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

    def cloneWith(cgen: CodeGenerationContext): SdagGenerationContext = {
      SdagGenerationContext(cgen, this.table, this.continuation)
    }

    def cloneWith(table: SymbolTable): SdagGenerationContext = {
      SdagGenerationContext(this.cgen, table, this.continuation)
    }

    def cloneWith(
        cgen: CodeGenerationContext,
        table: SymbolTable
    ): SdagGenerationContext = {
      SdagGenerationContext(cgen, table, this.continuation)
    }
  }

  def current(implicit ctx: CodeGenerationContext): String = _current
    .map(ctx.nameFor(_))
    .map(s => s.substring(0, s.length - 1))
    .getOrElse("")

  def subcontext: CodeGenerationContext =
    _current.flatMap(generated.get).getOrElse(???)

  def prefixFor(construct: Construct, start: Boolean = true)(implicit
      ctx: CodeGenerationContext
  ): String = {
    current + {
      construct match {
        case s: Loop => s"loop_${s.id}_${if (start) "header" else "latch"}"
        case _       => s"block_${construct.id}"
      }
    } + "__"
  }

  def forwardState(
      ctx: SdagGenerationContext,
      stateName: String,
      stackName: String
  ): Unit = {
    ctx.cgen << stateType << "(" << stateName << ".first, std::move(" << stackName << "))"
  }

  def continuation(
      ctx: SdagGenerationContext,
      to: Option[Any],
      arg: String,
      state: String,
      stack: String
  ): Unit = {
    val s = to.orElse(ctx.continuation).map {
      case x: Construct => prefixFor(x)(ctx.cgen)
      case x: String    => x
      case _            => ???
    }

    ctx.cgen << s << "(" << arg << "," << forwardState(
      ctx,
      state,
      stack
    ) << ");"
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
      ctx: CodeGenerationContext
  ): (String, String) = {
    val argName = "__arg__"
    val stateName = "__state__"
    ctx << "static" << "void" << name << "(" << "void*" << argName << "," << stateType << "&&" << stateName << ")" << "{"
    ctx << "auto*" << "self" << "=" << "(" << proxy.map(
      _.baseName
    ) << "*)hypercomm::access_context_();"
    (argName, stateName)
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
        ctx << "new hypercomm::typed_microstack<" << (decls.map(
          ctx.typeFor(_, _current)
        ), ",") << ">("
        ctx << s"$stateName.second.release(),"
        ctx << (decls.map(_ => "hypercomm::tags::no_init()"), ",") << ")"
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
        case x: EirDeclaration => Seq((x.name, x.declaredType))
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
    val ty = ctx.typeFor(decl._2, _current)
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
      ctx: CodeGenerationContext,
      blockName: String,
      decls: List[(String, EirResolvable[EirType])]
  ): Unit = {
    decls.headOption.foreach { case (declName, _) =>
      ctx << s"static constexpr auto ${offsetFor(blockName, declName)} = 0;"
    }
    if (decls.nonEmpty) decls.tail.indices.foreach { i =>
      ctx << s"static constexpr auto ${offsetFor(
        blockName,
        decls(i + 1)._1
      )} = ${offsetFor(blockName, decls(i)._1)} + sizeof(${ctx.typeFor(decls(i)._2)});"
    }
  }

  def visit(
      _ctx: SdagGenerationContext,
      block: SerialBlock
  ): SymbolTable = {
    val decls = collectDeclarations(block.slst)
    val blockName = prefixFor(block)(_ctx.cgen)
    val blockTable = _ctx.table :+ (blockName, decls)
    val sctx = _ctx.cloneWith(subcontext, blockTable)
    declareOffsets(sctx.cgen, blockName, decls)
    val (argName, stateName) = functionHeader(blockName, _ctx.proxy, sctx.cgen)
    val stkName = functionStack(sctx.cgen, decls.map(_._2), stateName)
    sctx.cgen << "auto*" << "__server__" << "=" << s"(std::shared_ptr<$serverType>*)$argName;"
    sctx.enter(stkName)
    block.slst.foreach {
      case d: EirDeclaration =>
        val (ref, ty) =
          derefVariable(sctx.cgen, blockName, (d.name, d.declaredType), stkName)
        sctx.cgen << "new (&(" << ref << ")) " << ty << "(" << d.initialValue
          .foreach(GenerateCpp.visit(_)(sctx.cgen)) << ");"
      case n: EirNode => sctx.cgen << GenerateCpp.visit(n)(sctx.cgen) << ";"
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
    val (headerTable, argName) = visitLoopHeader(
      SdagGenerationContext(subcontext, ctx.table),
      loop,
      ctx.proxy
    )
    val sctx = SdagGenerationContext(
      subcontext,
      headerTable,
      Some(prefixFor(loop, start = false)(subcontext))
    )
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
      wrapperName: Option[String]
  )(
      ctx: SdagGenerationContext
  ): String = {
    val list = GenerateCpp.generateRequestList(when)(ctx.cgen)
    val ty = GenerateCpp.joinRequestTypes(list)
    val com = "__com__"
    val set = "__set__"
    val setTy = s"${set.substring(0, set.length - 1)}type__"

    ctx.cgen << "using" << setTy << "=" << ty << ";"
    ctx.cgen << "auto" << com << "=" << s"ergoline::make_component<$setTy, $argType>(*" << ctx.cgen.currentProxySelf << ","
    ctx.cgen << wrapperName << "," << "std::make_tuple(" << argName << s", $stateName.first, " << stackName << ")"
    ctx.cgen << ");"

    GenerateCpp.generateRequest(when, list, com)(ctx.cgen)

    ty
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
    declareOffsets(sctx.cgen, name, decls)
    val (argName, stateName) = functionHeader(name, _ctx.cgen.proxy, sctx.cgen)
    val stkName =
      functionStack(sctx.cgen, decls.map(_._2), stateName, shared = true)
    val wrapperName = clause.body
      .map(prefixFor(_)(sctx.cgen))
      .map(s => s.substring(0, s.length - 1) + "wrapper__")
    sctx.enter(stkName)
    val reqTy = clause.node match {
      case x: EirSdagWhen =>
        makeRequest(clause, x, stateName, stkName, argName, wrapperName)(sctx)
    }
    sctx.leave()
    sctx.cgen << "}"
    clause.body.foreach(visit(_ctx.cloneWith(table), _))

    sctx.cgen << "static" << "void" << wrapperName << "(" << reqTy << "&" << "__set__" << "," << argType << "&&" << argName << ")" << "{"

    sctx.cgen << "}"

    _ctx.table
  }

  def visitLoopHeader(
      _ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy]
  ): (SymbolTable, String) = {
    val blockName = prefixFor(loop)(_ctx.cgen)
    val decls = collectDeclarations(loop.node match {
      case x: EirForLoop => x.header match {
          case y: EirCStyleHeader => y.declaration
          case y: EirForAllHeader => y.declaration
        }
      case _ => None
    })
    val blockTable = _ctx.table :+ (blockName, decls)
    val ctx = _ctx.cloneWith(blockTable)
    declareOffsets(ctx.cgen, blockName, decls)
    val (argName, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx.cgen, decls.map(_._2), stateName)
    ctx.enter(stkName)
    ctx.cgen << "if" << "("
    loop.node match {
      case x: EirForLoop => x.header match {
          case EirCStyleHeader(_, test, _) => GenerateCpp.visit(test)(ctx.cgen)
        }
    }
    ctx.cgen << ")" << "{"
    continuation(ctx, loop.body, argName, stateName, stkName)
    ctx.cgen << "}" << "else" << "{"
    continuation(ctx, loop.successors, argName, stateName, stkName)
    ctx.cgen << "}" << "}"
    ctx.leave()
    (blockTable, argName)
  }

  def visitLoopLatch(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy],
      argName: String
  ): Unit = {
    val blockName = prefixFor(loop, start = false)(ctx.cgen)
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
      Some(prefixFor(loop)(ctx.cgen)),
      argName,
      stateName,
      stkName
    )
    ctx.leave()
    ctx.cgen << "}"
  }

  def visit(
      ctx: SdagGenerationContext,
      construct: Construct
  ): SymbolTable = {
    val next = construct match {
      case x: SerialBlock => visit(ctx, x)
      case x: Loop        => visit(ctx, x)
      case x: Clause      => visit(ctx, x)
      case _              => ctx.table
    }

    if (construct.successors.size <= 1) {
      construct.successors.foldLeft(next)((next, nextConstruct) => {
        visit(ctx.cloneWith(next), nextConstruct)
      })
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
      val table: SymbolTable = Nil
      res.foldLeft(table)((table, construct) => {
        visit(SdagGenerationContext(ctx, table), construct)
      })
      _current = None
      ctx << "}"
      true
    }
  }
}
