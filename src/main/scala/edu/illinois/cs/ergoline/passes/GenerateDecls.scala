package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.GenerateCpp.{makePupper, nameFor, templatedNameFor, visitInherits, visitTemplateArgs}
import edu.illinois.cs.ergoline.resolution.Find

object GenerateDecls {
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = this.visit

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
    ctx << ctx.typeFor(x.declaredType) << nameFor(ctx, x) << ";"
  }

  def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace " << ctx.nameFor(x) << "{" << x.children << "}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    if (x.annotation("system").isDefined) return
    ctx << visitTemplateArgs(ctx, x.templateArgs) << s"struct ${nameFor(ctx, x)}" << visitInherits(ctx, x) << "{" << {
      if (x.isInstanceOf[EirTrait]) {
        if (x.templateArgs.isEmpty) {
          List(s"static ${nameFor(ctx, x)}* fromPuppable(ergoline::puppable *p);")
        } else {
          // TODO fix to call makeFromPuppable
          Nil
        }
      } else {
        makePupper(ctx, x)
        // TODO PUPable_decl_base_template
        List(if (x.templateArgs.isEmpty) s"PUPable_decl_inside(${nameFor(ctx, x)});"
        else s"PUPable_decl_inside_template(${templatedNameFor(ctx, x)});",
          s"${nameFor(ctx, x)}(CkMigrateMessage *m) : ergoline::puppable(m) { }",
          "virtual ergoline::puppable* toPuppable() override { return this; }") ++
          Find.traits(x).map(x => s"friend class ${nameFor(ctx, x)};")
      }
    } << x.members << s"};"
  }

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit =
    GenerateCpp.visitFunction(ctx, x, isMember = true)
}
