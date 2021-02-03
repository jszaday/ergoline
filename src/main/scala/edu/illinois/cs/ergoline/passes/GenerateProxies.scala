package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp._
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

object GenerateProxies {

  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = GenerateCpp.visit

  def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val ns = x.namespaces.toList
    ctx << ns.map(ns => s"namespace ${ctx.nameFor(ns)} {") << {
      ctx.updateProxy(x)
      if (x.isAbstract) visitAbstractProxy(ctx, x)
      else visitConcreteProxy(ctx, x)
      ctx.updateProxy(x)
    } << ns.map(_ => "}")
  }

  def visitAbstractPup(ctx: CodeGenerationContext, numImpls: Int): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{" << s"p | handle;" << {
      (0 until numImpls).map(x => s"p | p$x;")
    } << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction, impls: List[EirMember]): Unit = {
    val args = f.functionArgs
    val name = ctx.nameFor(f)
    val nArgs = args.map(ctx.nameFor(_)).mkString(", ")
    // TODO support non-void returns?
    ctx << s"void $name(" << (args, ", ") << ")" << "{" << {
      List(s"switch (handle)", "{") ++
        impls.zipWithIndex.map {
          case (m, x) => s"case $x: { p$x.${ctx.nameFor(m)}($nArgs); break; }"
        } ++
        List("default: { CkAbort(\"abstract proxy unable to find match\"); }", "}", "}")
    }
  }

  def makeHasher(ctx: CodeGenerationContext, numImpls: Int): Unit = {
    ctx << "virtual std::size_t hash() override" << "{"
    ctx << "ergoline::hasher _;"
    ctx << "switch (handle)" << "{"
    (0 until numImpls).foreach(x => {
      ctx << s"case $x: { _ | p$x; break; }"
    })
    ctx << "default: { CkAbort(\"abstract proxy unable to find match\"); }"
    ctx << "}"
    ctx << "return _.hash();"
    ctx << "}"
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = ctx.nameFor(x)
    val impls = x.derived.toList
    val implNames = impls.map(ctx.nameFor(_))
    ctx << s"struct $name: public ergoline::hashable" << "{" << s"int handle;" << {
      implNames.zipWithIndex.flatMap({
          case (derived, idx) =>
            List(s"$derived p$idx;", s"$name($derived x) : handle($idx), p$idx(x) { }")
        }) ++ List(s"$name() : handle(-1) { }")
    } << visitAbstractPup(ctx, impls.length) << {
      x.members.foreach(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member),
        impls.map(y => Find.namedChild[EirMember](Some(y), x.name))))
    } << {
      makeHasher(ctx, impls.length)
    } << "};"
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val base = ctx.nameFor(x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    GenerateCpp.visitTemplateArgs(ctx, x.templateArgs)
    val args = if (x.templateArgs.nonEmpty) GenerateCpp.templateArgumentsToString(ctx, x.templateArgs, None) else ""
    ctx << s"struct $name: public CBase_$name$args" << "{" << {
      ctx << "void pup(PUP::er &p)" << "{" << {
        pupperFor((ctx, x, "p"))("impl_", x.base)
      } << {
        x.members.filter(_.isMailbox).map(mailboxName(ctx, _)._1).map("p | " + _ + ";")
      } << "}"; ()
    } << {
      x.membersToGen
        .foreach(x => visitProxyMember(ctx, x))
    } << "std::shared_ptr<" << nameFor(ctx, x.base, includeTemplates = true) << ">" << s" impl_;" << s"};"
  }

  def makeArgsVector(ctx: CodeGenerationContext, name: String): Unit = {
    assert(name != "msg" && name != "_argc_")
    ctx << "std::array<std::size_t, 1> _argc_ = {(std::size_t) msg->argc};"
    ctx << s"auto $name = std::make_shared<ergoline::array<std::string, 1>>(_argc_);"
    ctx << s"for (auto i = 0; i < args->size(); i++) { new (&(*$name)[i]) std::string(msg->argv[i]); }"
  }

  def makePointerRhs(ctx: (CodeGenerationContext, EirNode))(current: String, expected: EirType): String = {
    expected match {
      case t: EirTupleType =>
        "std::make_tuple(" + {
          t.children.zipWithIndex.map({
            case (r, idx) =>
              makePointerRhs(ctx)(s"std::get<$idx>($current)", Find.uniqueResolution(r))
          }).mkString(", ")
        } + ")"
      case t if needsCasting(t) =>
        s"ergoline::from_pupable<${ctx._1.nameFor(expected, Some(ctx._2))}>($current)"
      case _ => current
    }
  }

  def makeSmartPointer(ctx: CodeGenerationContext)(x: EirFunctionArgument): Unit = {
    val ty: EirType = Find.uniqueResolution(x.declaredType)
    if (containsArray(ctx, ty)) {
      ctx << ctx.typeFor(ty, Some(x)) << ctx.nameFor(x) << "=" << {
        unflattenArgument((ctx, x), x.name, ty)
      } << ";"
    } else if (needsCasting(ty)) {
      ctx << ctx.typeFor(ty, Some(x)) << ctx.nameFor(x) << "=" << {
        makePointerRhs((ctx, x))(s"${x.name}_", ty)
      } << ";"
    }
  }

  def needsCasting(n: EirNode): Boolean = {
    n match {
      // TODO use specialization
      case _: EirTemplateArgument => false
      case _: EirLambdaType => false
      case t: EirTupleType => t.children.map(Find.uniqueResolution[EirType]).exists(needsCasting)
      case t: EirType => t.isTrait && t.isPointer && !t.isSystem
      case _ => Errors.incorrectType(n, classOf[EirType])
    }
  }

  private def arraySizes(ctx: CodeGenerationContext, name: String, t: EirType): List[String] = {
    val nd = arrayDim(ctx, t).getOrElse(Errors.unreachable())
    (0 until nd).map(name + "_sz" + _).toList
  }

  def flattenArgument(ctx: (CodeGenerationContext, EirNode), name: String, ty: EirType): String = {
    ty match {
      case t: EirTupleType if containsArray(ctx._1, t) => t.children.zipWithIndex.map {
        case (ty, idx) => flattenArgument(ctx, s"${name}_$idx", ctx._1.resolve(ty))
      } mkString ", "
      case t if isArray(ctx._1, t) =>
        val names = arraySizes(ctx._1, name, ty)
        names.map("int " + _).mkString(", ") + s", ${GenerateCpp.typeForEntryArgument(ctx)(arrayElementType(t))}" + {
          if (ctx._1.language == "ci") s" ${name}_arr[${names mkString " * "}]"
          else s"* ${name}_arr"
        }
      case _ => s"${GenerateCpp.typeForEntryArgument(ctx)(ty)} $name"
    }
  }

  def unflattenArgument(ctx: (CodeGenerationContext, EirNode), name: String, ty: EirType): String = {
    ty match {
      case t: EirTupleType if containsArray(ctx._1, t) =>
        s"std::make_tuple(${t.children.zipWithIndex.map {
          case (ty, idx) => unflattenArgument(ctx, s"${name}_$idx", ctx._1.resolve(ty))
        } mkString ", "})"
      case t if isArray(ctx._1, t) =>
        val names = arraySizes(ctx._1, name, ty)
        val index = s"(std::size_t) ${names mkString ", (std::size_t) "}"
        val arrTy = ctx._1.nameFor(t, Some(ctx._2))
        s"std::make_shared<$arrTy>(std::shared_ptr<void>{}, ${name}_arr, $index)"
      case t if needsCasting(t) => makePointerRhs(ctx)(name, t)
      case _ => name
    }
  }

  def visitFunctionArgument(ctx: CodeGenerationContext, arg: EirFunctionArgument): Unit = {
    val ty = Find.uniqueResolution[EirType](arg.declaredType)
    if (GenerateCpp.containsArray(ctx, ty)) {
      ctx << flattenArgument((ctx, arg), arg.name, ty)
    } else {
      GenerateCpp.visitFunctionArgument(ctx, arg)
      if (needsCasting(ty)) ctx.append("_")
    }
  }

  def visitFunctionArguments(ctx: CodeGenerationContext, args: List[EirFunctionArgument]): Unit = {
    if (args.nonEmpty) {
      for (arg <- args.init) {
        visitFunctionArgument(ctx, arg)
        ctx << ","
      }
      visitFunctionArgument(ctx, args.last)
    }
  }

  private def makeEntryBody(ctx: CodeGenerationContext, member: EirMember): Unit = {
    member.counterpart match {
      case Some(m: EirMember) if m.isMailbox => makeMailboxBody(ctx, member)
      case Some(m@EirMember(_, f: EirFunction, _)) =>
        if (m.isEntryOnly) {
          ctx << "(([&](void) mutable" << visitFunctionBody(ctx, f) << ")())"
        } else {
          ctx << s"this->impl_->${ctx.nameFor(f)}(${f.functionArgs.map(ctx.nameFor(_)).mkString(", ")})"
        }
      case _ => Errors.unreachable()
    }
  }

  def mailboxName(ctx: CodeGenerationContext, node: EirNode, types: List[String]): String = {
    // TODO impl this
    ctx.nameFor(asMember(Some(node)).getOrElse(node)) + "_mailbox_"
  }

  def mailboxName(ctx: CodeGenerationContext, x: EirMember): (String, List[String]) = {
    val f = assertValid[EirFunction](x.member)
    val tys = f.functionArgs.map(_.declaredType).map(ctx.typeFor(_, Some(x)))
    (mailboxName(ctx, x, tys), tys)
  }

  def makeMailboxDecl(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val (name, tys) = mailboxName(ctx, x)
    ctx << s"ergoline::mailbox<${tys mkString ", "}> $name;"
  }

  def makeMailboxBody(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val f = assertValid[EirFunction](x.member)
    val args = f.functionArgs.map(ctx.nameFor(_))
    val name = mailboxName(ctx, x)._1
    ctx << s"auto __value__ = std::make_shared<decltype($name)::tuple_t>(std::make_tuple(" << (args, ",") << "));"
    ctx << s"$name.put(__value__);"
  }

  def visitProxyMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
      val proxy = x.parent.to[EirProxy]
      val isConstructor = x.isConstructor
      val base = proxy.map(x => nameFor(ctx, x.base, includeTemplates = true)).getOrElse("")
      val f = assertValid[EirFunction](x.member)
      val isMain = proxy.exists(_.isMain)
      val isAsync = x.annotation("async").isDefined
      val args = f.functionArgs
      if (x.isMailbox) makeMailboxDecl(ctx, x)
      if (isAsync) ctx << "void"
      else if (!isConstructor) ctx << ctx.typeFor(f.returnType)
      ctx << {
        if (isConstructor) {
          val baseName = proxy.map(x => ctx.nameFor(x.base)).getOrElse("")
          s"${baseName}_${proxy.flatMap(_.collective).map(x => s"${x}_").getOrElse("")}"
        } else {
          ctx.nameFor(f)
        }
      } << "(" << {
        if (isMain && isConstructor) { ctx << "CkArgMsg* msg"; () }
        else {
          if (isAsync) {
            ctx << ctx.typeFor(f.returnType) << ctx.temporary
            if (args.nonEmpty) ctx << ","
          }
          visitFunctionArguments(ctx, args)
        }
      } << ")" << "{" << {
        if (isConstructor && isMain) args.headOption.foreach(x => makeArgsVector(ctx, x.name))
        else args.foreach(makeSmartPointer(ctx))
      }
      if (isConstructor) {
        x.counterpart match {
          case Some(m) if m.isEntryOnly =>
            ctx << "this->impl_ = std::make_shared<" << base << ">((CkMigrateMessage *)nullptr);"
            makeEntryBody(ctx, x)
            ctx << ";"
          case _ =>
            ctx << "this->impl_ = std::make_shared<" << base << ">(" << (args.map(ctx.nameFor(_)), ", ") << ");"
        }
      } else {
        if (isAsync) {
          ctx << ctx.temporary << ".set("
        } else if (ctx.resolve(f.returnType) != globals.typeFor(EirLiteralTypes.Unit)) {
          ctx << "return "
        }
        makeEntryBody(ctx, x)
        if (isAsync) ctx << ");"
        else ctx << ";"
      }
      ctx << "}"
  }

}
