package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.GenerateCpp.{isTransient, makeHasher, makePupper, nameFor, visitInherits, visitTemplateArgs}
import edu.illinois.cs.ergoline.resolution.Find

object GenerateDecls {
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = this.visit

  def hasHash(x: EirClassLike): Boolean = false
  def hasPup(x: EirClassLike): Boolean = false

  def visit(ctx: CodeGenerationContext, node: EirNode): Unit = {
    node match {
      case x: EirNamespace => visitNamespace(ctx, x)
      case x: EirClassLike => visitClassLike(ctx, x)
      case x: EirMember => visit(ctx, x.member)
      case x: EirFunction => visitFunction(ctx, x)
      case x: EirDeclaration => visitDeclaration(ctx, x)
      case x: EirFunctionArgument => GenerateCpp.visitFunctionArgument(ctx, x)
      case _: EirImport =>
      case _: EirFileSymbol =>
    }
  }

  def visitDeclaration(ctx: CodeGenerationContext, x: EirDeclaration): Unit = {
    ctx << ctx.typeFor(x.declaredType, Some(x)) << nameFor(ctx, x) << ";"
  }

  def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace " << ctx.nameFor(x) << "{" << x.children << "}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    if (x.annotation("system").isDefined) return
    ctx << visitTemplateArgs(ctx, x.templateArgs) << s"struct ${nameFor(ctx, x)}" << visitInherits(ctx, x) << "{" << {
      if (x.isInstanceOf[EirTrait]) {
        List(s"static std::shared_ptr<${nameFor(ctx, x, x.templateArgs.nonEmpty)}> fromPuppable(ergoline::puppable *p);")
      } else if (!isTransient(x)) {
        if (!hasPup(x)) makePupper(ctx, x)
        if (!hasHash(x)) makeHasher(ctx, x)
        // TODO PUPable_decl_base_template
        List(if (x.templateArgs.isEmpty) s"PUPable_decl_inside(${nameFor(ctx, x)});"
        else s"PUPable_decl_inside_template(${nameFor(ctx, x)});",
          s"${nameFor(ctx, x)}(CkMigrateMessage *m) : ergoline::puppable(m) { }",
          "virtual ergoline::puppable* toPuppable() override { return this; }") ++
          Find.traits(x).map(x => s"friend class ${nameFor(ctx, x)};")
      } else {
        if (!hasHash(x)) makeHasher(ctx, x)
        List("virtual ergoline::puppable* toPuppable() override { CkAbort(\"" + nameFor(ctx, x) + " is @transient and cannot be pup'd\"); return nullptr; }")
      }
    } << x.members << s"};"
  }

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit =
    GenerateCpp.visitFunction(ctx, x, isMember = true)
}
