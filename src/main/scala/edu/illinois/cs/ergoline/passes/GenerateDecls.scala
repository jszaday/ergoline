package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.{
  makeHasher,
  makePupper,
  visitInherits,
  visitTemplateArgs
}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike
import edu.illinois.cs.ergoline.util.{assertValid, isSystem}

object GenerateDecls {
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = this.visit

  def hasHash(x: EirClassLike): Boolean = false
  def hasPup(x: EirClassLike): Boolean = false

  def visit(ctx: CodeGenerationContext, node: EirNode): Unit = {
    node match {
      case x: EirNamespace        => visitNamespace(ctx, x)
      case x: EirClassLike        => visitClassLike(ctx, x)
      case x: EirMember           => visitMember(ctx, x)
      case x: EirFunction         => visitFunction(ctx, x)
      case x: EirDeclaration      => visitDeclaration(ctx, x)
      case x: EirFunctionArgument => GenerateCpp.visitFunctionArgument(x)(ctx)
      case _: EirImport           =>
      case _: EirTypeAlias        =>
      case _: EirFileSymbol       =>
    }
  }

  def visitMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
    ctx << Option.when(x.member match {
      case _: EirClassLike | _: EirTypeAlias => false
      case _                                 => x.isStatic
    })({
      if (x.member.isInstanceOf[EirDeclaration]) "thread_local static"
      else "static"
    })

    visit(ctx, x.member)
  }

  def visitDeclaration(ctx: CodeGenerationContext, x: EirDeclaration): Unit = {
    ctx << ctx.typeFor(x.declaredType, Some(x)) << ctx.nameFor(x) << ";"
  }

  def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace " << ctx.nameFor(x) << "{" << x.children << "}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    if (isSystem(x)) {
      x.members.collect {
        case m @ EirMember(_, x: EirNode, _) if !isSystem(x) => x
      } foreach { x => visit(ctx, x) }
      return
    }

    val thisName = ctx.nameFor(x)
    val declName = GenerateCpp.declNameFor(x)(ctx)

    ctx << visitTemplateArgs(x)(ctx) << "struct" << declName << visitInherits(
      x
    )(ctx) << "{"
    if (!x.isInstanceOf[EirTrait]) {
      if (!hasHash(x)) makeHasher(ctx, x)

      if (!hasPup(x)) {
        if (x.templateArgs.nonEmpty || x.isTransient) {
          makePupper(ctx, x, isMember = true)
        } else {
          ctx << "virtual void __pup__(hypercomm::serdes&) override;"
        }
      }

      val parent = x.extendsThis
        .map(Find.uniqueResolution[EirType])
        .map(ctx.nameFor(_, Some(x)))

      if (x.isValueType) { // TODO add !isAbstract?
        val tmp = ctx.temporary
        ctx << declName << "(" << "const" << thisName << "&" << tmp << ")" << {
          parent.map(p => s": $p($tmp)")
        } << "{"
        x.members.collect {
          case m @ EirMember(_, d: EirDeclaration, _) if !m.isStatic => d.name
        } foreach { x =>
          ctx << "this" << "->" << x << "=" << tmp << "." << x << ";"
        }
        ctx << "}"
      }

      val fields = x.members collect {
        case m @ EirMember(_, d: EirDeclaration, _) if !m.isStatic => d
      } filter { x =>
        ctx.resolve(x.declaredType).isReconstructible
      }

      val needsColon = parent.nonEmpty || fields.nonEmpty

      ctx << declName << "(PUP::reconstruct __tag__)" << Option.when(
        needsColon
      )(":") << parent.map(p => s"$p(__tag__)") << Option.when(
        parent.nonEmpty && fields.nonEmpty
      )(",") << {
        (fields.map(x => s"${x.name}(__tag__)"), ",")
      } << "{}"
    }
    ctx << x.members << s"};"

    x.members
      .collect {
        case m @ EirMember(_, _: EirDeclaration, _) if m.isStatic => m
      }
      .foreach(outsideStaticDecl(ctx, _))
  }

  def outsideStaticDecl(ctx: CodeGenerationContext, m: EirMember): Unit = {
    val decl: EirDeclaration = assertValid[EirDeclaration](m.member)
    ctx << visitTemplateArgs(m.base.templateArgs)(ctx)
    ctx << "thread_local" << ctx.typeFor(
      decl.declaredType,
      Some(decl)
    ) << GenerateCpp.nameFor(
      ctx,
      m.base,
      includeTemplates = true
    ) << "::" << m.name
    ctx << decl.initialValue.map(_ => "=") << decl.initialValue << ";"
  }

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit =
    GenerateCpp.visitFunction(x, isMember = true)(ctx)
}
