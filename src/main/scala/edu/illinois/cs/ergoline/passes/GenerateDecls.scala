package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.{
  makeHasher,
  makePupper,
  templateArgsOf,
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

  def visitPredicate(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    val args = GenerateCpp.templateArgsOf(x)
    x.predicate match {
      case Some(predicate) =>
        ctx << "<" << (args.map(ctx.nameFor(_)), ",")
        ctx << Option.when(args.nonEmpty)(",")
        ctx << "typename" << "std::enable_if" << "<" << {
          StaticGenerator.visit(predicate)
        } << ">::type" << ">"
      case None =>
    }
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    val checked = ctx.hasChecked(x)
    if (isSystem(x) || !checked) {
      if (checked) {
        x.members.collect {
          case EirMember(_, x: EirNode, _) if !isSystem(x) => x
        } foreach { x => visit(ctx, x) }
      }

      return
    }

    val thisName = ctx.nameFor(x)
    val declName = GenerateCpp.declNameFor(x)(ctx)

    ctx << {
      visitTemplateArgs(x)(ctx)
    } << "struct" << declName << {
      visitPredicate(x)(ctx)
    } << {
      visitInherits(x)(ctx)
    } << "{"

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
        val objBase = "std::shared_ptr<ergoline::object_base_>"
        ctx << "virtual" << objBase << "__this_object__" << "(" << "void" << ")" << "override" << "{"
        ctx << "return" << objBase << "(" << objBase << "{},this);"
        ctx << "}"

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
      } filter { x => ctx.resolve(x.declaredType).isReconstructible }

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

  private val ns = globals.ergolineModule

  def mkIteratorAccessor(
      x: EirClass,
      iter: Option[EirMember],
      args: List[EirTemplateArgument],
      qualifiedName: String
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "constexpr" << "auto" << "accessor" << "="
    if (x.isSystem) {
      ctx << iter.map(ctx.nameFor(_, ns))
      ctx << Option.when(args.nonEmpty)(
        s"<${args.map(ctx.nameFor(_)) mkString ","}>"
      )
    } else if (x.isValueType) {
      ctx << "ergoline::access_value_iter<" << qualifiedName << ">"
    } else {
      ctx << "ergoline::access_ref_iter<" << qualifiedName << ">"
    }
    ctx << ";"
  }

  def mkIteratorBridge(
      x: EirType,
      y: EirTemplatedType
  )(implicit ctx: CodeGenerationContext): Unit = {
    val cls = assertValid[EirClass](Find.asClassLike(x))
    val accessor = CheckTypes.mkAccessor[EirMember](cls, "iter")(None)
    val iter = Find
      .resolveAccessor(accessor)(Some(x), Some(false))(ctx.tyCtx)
      .headOption
      .map(_._1)

    if (
      !iter.exists(z =>
        isSystem(z) || z.isAbstract || {
          ctx.hasChecked(z.member.asInstanceOf[EirSpecializable])
        }
      )
    ) {
      return
    }

    val args = templateArgsOf(cls)
    val qualifiedName = {
      GenerateCpp.qualifiedNameFor(
        ctx,
        ns.getOrElse(???),
        includeTemplates = true
      )(x)
    }

    ctx << visitTemplateArgs(args)
    ctx << "struct" << "iterator_for" << "<" << qualifiedName << ">" << "{"
    ctx << "using" << "value_type" << "=" << ctx.typeFor(
      y.types.head,
      ns
    ) << ";"
    ctx << "static" << mkIteratorAccessor(cls, iter, args, qualifiedName)(ctx)
    ctx << "};"
  }
}
