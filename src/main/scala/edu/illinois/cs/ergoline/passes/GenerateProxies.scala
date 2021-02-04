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
    // TODO support non-void returns?
    ctx << s"void $name(" << Option.when(args.nonEmpty)("CkMessage* __msg__") << ")" << "{" << {
      List(s"switch (handle)", "{") ++
        impls.zipWithIndex.map {
          case (m, x) => s"case $x: { p$x.${ctx.nameFor(m)}(__msg__); break; }"
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
    ctx << s"struct $name: public ergoline::hashable, public CProxy" << "{" << s"int handle;" << {
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

  def makeParameter(ctx: CodeGenerationContext, x: EirFunctionArgument): Unit = {
    val ty = ctx.resolve(x.declaredType)
    val s = ctx.typeFor(x.declaredType, Some(x))
    if (ty.isPointer) {
      ctx << s << x.name << ";"
    } else {
      ctx << "char" << s"__${x.name}__" << s"[sizeof($s)];"
      ctx << s"auto& ${x.name} = reinterpret_cast<$s&>(__${x.name}__);"
    }
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
        if (isMain && isConstructor) {
          ctx << "CkArgMsg* msg";
        } else {
          ctx << Option.when(args.nonEmpty || isAsync)("CkMessage* __msg__")
        }
      } << ")" << "{"
      if (isConstructor && isMain) {
        args.headOption.foreach(x => makeArgsVector(ctx, x.name))
      } else if (args.nonEmpty || isAsync) {
        if (isAsync) {
          ctx << ctx.typeFor(f.returnType) << "__future__" << "("  << "PUP::reconstruct{})" << ";"
        }
        args.foreach(makeParameter(ctx, _))
        ctx << "ergoline::unpack(__msg__," << (Option.when(isAsync)("__future__") ++ args.map(_.name), ",") << ");"
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
          ctx << "__future__.set" << "("
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
