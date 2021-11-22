package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.analysis.Segmentation._
import edu.illinois.cs.ergoline.ast.{
  EirDeclaration,
  EirFunction,
  EirMember,
  EirMultiDeclaration
}

import scala.collection.mutable

object GenerateSdag {
  val serverType = "hypercomm::state_server<hypercomm::microstack>"
  val stateType = s"typename $serverType::state_type"

  val generated: mutable.Map[EirMember, CodeGenerationContext] = mutable.Map()
  private var _current: Option[EirMember] = None

  def current(implicit ctx: CodeGenerationContext): String = _current
    .map(ctx.nameFor(_))
    .map(s => s.substring(0, s.length - 1))
    .getOrElse("")

  def subcontext: CodeGenerationContext =
    _current.flatMap(generated.get).getOrElse(???)

  def continuation(from: Construct, to: Construct, state: String)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    ctx << (to match {
      case s: ScopingConstruct => s"// cluster_${s.id} ;"
      case _                   => s"block_${to.id}"
    })
  }

  def visit(block: SerialBlock)(implicit ctx: CodeGenerationContext): Unit = {
    val stateName = "__state__"
    val sctx = subcontext
    sctx << s"static void ${current}block_${block.id}(void* __arg__, $stateType&& $stateName)" << "{"
    sctx << "auto*" << "__server__" << "=" << s"(std::shared_ptr<$serverType>*)__arg__;"
    sctx << "auto*" << "self" << "=" << "(" << ctx.proxy.map(
      _.baseName
    ) << "*)hypercomm::access_context_();"
    val decls = block.slst.collect {
      case x: EirDeclaration => Seq((x.name, x.declaredType))
      case x: EirMultiDeclaration =>
        x.children.map(c => (c.name, c.declaredType))
    }.flatten
    if (decls.isEmpty) {
      sctx << s"auto& stk = $stateName.second;"
    } else {
      sctx << s"auto* stk = new hypercomm::typed_microstack<" << (decls
        .map(_._2)
        .map(ctx.typeFor(_, _current)), ",") << ">("
      sctx << s"$stateName.second.release(),"
      sctx << (decls.map(_ => "hypercomm::tags::no_init()"), ",") << ");"
    }
    sctx.pushProxySelf("self")

    block.successors.foreach(continuation(block, _, stateName)(sctx))
    sctx.popProxySelf()
    sctx << "}"
  }

  def visit(construct: Construct)(implicit ctx: CodeGenerationContext): Unit = {
    construct match {
      case x: SerialBlock => visit(x)
      case _              => "TODO."
    }

    construct.successors.foreach(visit)
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
      res.foreach(visit(_))
      _current = None
      ctx << "}"
      true
    }
  }
}
