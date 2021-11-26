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
  EirNode
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

  def continuation(to: Construct, state: String, stack: String)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    ctx << "//" << prefixFor(to) << "(...);"
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
      stateName: String
  ): String = {
    val stkName = "__stk__"
    if (decls.isEmpty) {
      ctx << s"auto& $stkName = $stateName.second;"
    } else {
      ctx << s"auto* $stkName = new hypercomm::typed_microstack<" << (decls.map(
        ctx.typeFor(_, _current)
      ), ",") << ">("
      ctx << s"$stateName.second.release(),"
      ctx << (decls.map(_ => "hypercomm::tags::no_init()"), ",") << ");"
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
      )} = ${offsetFor(blockName, decls(i)._1)} + sizeof(${decls(i)._2});"
    }
  }

  def visit(
      ctx: SdagGenerationContext,
      block: SerialBlock
  ): SymbolTable = {
    val sctx = subcontext
    val decls = collectDeclarations(block.slst)
    val blockName = prefixFor(block)(ctx.cgen)
    val blockTable = ctx.table :+ (blockName, decls)
    declareOffsets(sctx, blockName, decls)
    val (argName, stateName) = functionHeader(blockName, ctx.proxy, sctx)
    val stkName = functionStack(sctx, decls.map(_._2), stateName)
    sctx << "auto*" << "__server__" << "=" << s"(std::shared_ptr<$serverType>*)$argName;"
    ctx.enter(stkName)
    block.slst.foreach {
      case d: EirDeclaration =>
        val (ref, ty) =
          derefVariable(ctx.cgen, blockName, (d.name, d.declaredType), stkName)
        sctx << "new (&(" << ref << ")) " << ty << "(" << d.initialValue
          .foreach(GenerateCpp.visit(_)(sctx)) << ");"
      case n: EirNode => sctx << GenerateCpp.visit(n)(sctx) << ";"
    }
    block.successors.foreach(continuation(_, stateName, stkName)(sctx))
    ctx.leave()
    sctx << "}"
    blockTable
  }

  def visit(
      ctx: SdagGenerationContext,
      loop: Loop
  ): SymbolTable = {
    val (_, headerTable) = visitLoopHeader(
      SdagGenerationContext(subcontext, ctx.table),
      loop,
      ctx.proxy
    )
    val sctx = SdagGenerationContext(
      subcontext,
      headerTable,
      Some(prefixFor(loop, start = false)(subcontext))
    )
    loop.body.map(visit(sctx, _))
    ctx.table
  }

  def visitLoopHeader(
      ctx: SdagGenerationContext,
      loop: Loop,
      proxy: Option[EirProxy]
  ): (String, SymbolTable) = {
    val blockName = prefixFor(loop)(ctx.cgen)
    val decls = collectDeclarations(loop.node match {
      case x: EirForLoop => x.header match {
          case y: EirCStyleHeader => y.declaration
          case y: EirForAllHeader => y.declaration
        }
      case _ => None
    })
    val blockTable = ctx.table :+ (blockName, decls)
    declareOffsets(ctx.cgen, blockName, decls)
    val (_, stateName) = functionHeader(blockName, proxy, ctx.cgen)
    val stkName = functionStack(ctx.cgen, decls.map(_._2), stateName)
    ctx.enter(stkName)
    ctx.cgen << "if" << "("
    loop.node match {
      case x: EirForLoop => x.header match {
          case EirCStyleHeader(_, test, _) => GenerateCpp.visit(test)(ctx.cgen)
        }
    }
    ctx.cgen << ")" << "{"
    loop.body.foreach(continuation(_, stateName, stkName)(ctx.cgen))
    ctx.cgen << "}" << "else" << "{"
    loop.successors.foreach(continuation(_, stateName, stkName)(ctx.cgen))
    ctx.cgen << "}" << "}"
    ctx.leave()
    (blockName, blockTable)
  }

  def visit(
      ctx: SdagGenerationContext,
      construct: Construct
  ): SymbolTable = {
    val next = construct match {
      case x: SerialBlock => visit(ctx, x)
      case x: Loop        => visit(ctx, x)
      case _              => ctx.table
    }

    if (construct.successors.size <= 1) {
      construct.successors.foldLeft(next)((next, nextConstruct) => {
        visit(SdagGenerationContext(ctx.cgen, next), nextConstruct)
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
