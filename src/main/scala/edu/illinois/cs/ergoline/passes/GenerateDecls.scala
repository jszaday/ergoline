package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.passes.GenerateCpp.{makePupper, nameFor, templatedNameFor, typeFor, visitFunctionArgument, visitInherits, visitTemplateArgs}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

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
    ctx << typeFor(ctx, x.declaredType) << nameFor(ctx, x) << ";"
  }

  def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace " << ctx.nameFor(x) << "{" << x.children << "}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    if (x.annotation("system").isDefined) return
    ctx << visitTemplateArgs(ctx, x.templateArgs) << s"struct ${nameFor(ctx, x)}" << visitInherits(ctx, x) << "{" << {
      if (x.isInstanceOf[EirTrait]) {
        List(s"static ${templatedNameFor(ctx, x)}* fromPuppable(ergoline::puppable *p);")
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

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    val member = x.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val virtual = Option.when(member.exists(_.isVirtual))("virtual")
    val overrides = Option.when(member.exists(_.isOverride))(" override")
    val isConstructor = member.exists(_.isConstructor)
    val retTy = Option.when(!isConstructor)(typeFor(ctx, x.returnType))
    ctx << virtual << retTy << ctx.nameFor(x) << "(" << (x.functionArgs, ", ") << ")" << overrides
    if ((parent.exists(_.templateArgs.nonEmpty) || x.templateArgs.nonEmpty) && x.body.isDefined) {
//      GenerateCpp.visitFunction(ctx, x, isMember = true)
    } else {
      ctx << (x.body.map(_ => ";").orElse(virtual.map(_ => " = 0;")), ";")
    }
  }
}
