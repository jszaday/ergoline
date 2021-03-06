package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp._
import edu.illinois.cs.ergoline.proxies.EirProxy
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

  def visitAbstractPup(ctx: CodeGenerationContext, indices: List[String]): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{"
    ctx << "CProxy::pup(p);"
    ctx << s"p | __id__;"
    ctx << "p | __msgType__;"
    ctx << indices.map(idx => s"p | $idx;")
    ctx << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction): Unit = {
    val args = f.functionArgs
    val name = ctx.nameFor(f)
    // TODO support non-void returns?
    // TODO make a helper function that does a "send-thru" operation
    ctx << s"void $name(" << Option.when(args.nonEmpty)("CkMessage* __msg__") << ")" << "{" << {
      ctx << "UsrToEnv(__msg__)->setMsgtype(__msgType__);"
      ctx << "CkSendMsg" << "(" << (name + "_idx__") << ",__msg__,&__id__,0" << ")" << ";"
    } << "}"
  }

  def makeHasher(ctx: CodeGenerationContext, indices: List[String]): Unit = {
    ctx << "virtual std::size_t hash() override" << "{"
    ctx << "ergoline::hasher _;"
    ctx << "_" << "|" << "__id__" << ";"
    ctx << "_" << "|" << "reinterpret_cast<std::underlying_type<CkEnvelopeType>::type&>(__msgType__)" << ";"
    indices.foreach(x => {
      ctx << "_" << "|" << x << ";"
    })
    ctx << "return _.hash();"
    ctx << "}"
  }

  def indexFor(ctx: CodeGenerationContext, proxy: EirProxy, function: EirFunction): String = {
    /* TODO FQN */ "CkIndex_" + proxy.baseName + "::idx_" + ctx.nameFor(function) + "_" + {
      if (function.functionArgs.nonEmpty) "CkMessage" else "void"
    } + "()"
  }

  def visitAbstractCons(ctx: CodeGenerationContext, base: EirProxy, derived: EirProxy): Unit = {
    val tmp = "__derived__"
    ctx << ctx.nameFor(base) << "(" << "const" << ctx.nameFor(derived, Some(base)) << "&" << tmp << ")"
    ctx << ":" << "CProxy" << "(" << tmp << ")" << "{"
    base.members.collect {
      case m@EirMember(_, f: EirFunction, _) if m.isEntry => derived.members.collectFirst {
        case n@EirMember(_, g: EirFunction, _) if n.isEntry && (f.name == g.name) && CheckFunctions.sharedArgs(ctx.typeContext, f, g) => (f, g)
      }
    }.flatten.foreach {
      case (f, g) => ctx << (ctx.nameFor(f) + "_idx__") << "=" << indexFor(ctx, derived, g) << ";"
    }
    ctx << "__id__" << "=" << tmp << "." << "ckGetChareID()" << ";"
    ctx << "__msgType__" << "=" << {
      (derived.isElement, derived.collective) match {
        case (false, None) => "ForChareMsg"
        case (_, _) => ???
      }
    } << ";"
    ctx << "}"
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = ctx.nameFor(x)
    val impls = x.derived.toList
    val indices = x.members.map(m => ctx.nameFor(m) + "_idx__")
    ctx << s"struct $name: public ergoline::hashable, public CProxy" << "{"
    ctx << "CkChareID" << "__id__" << ";"
    ctx << "CkEnvelopeType" << "__msgType__" << ";"
    indices.foreach(idx => ctx << "int" << idx << ";")
    ctx << name << "(" /* << TODO "PUP::reconstruct" */ << ")" << "{}"
    impls.collect { case p: EirProxy => p }.foreach(visitAbstractCons(ctx, x, _))
    ctx << visitAbstractPup(ctx, indices) << {
      x.members.foreach(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member)))
    } << {
      makeHasher(ctx, indices)
    } << "};"
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val base = ctx.nameFor(x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    GenerateCpp.visitTemplateArgs(ctx, x.templateArgs)
    val args = if (x.templateArgs.nonEmpty) GenerateCpp.templateArgumentsToString(ctx, x.templateArgs, None) else ""
    ctx << s"struct $name: public CBase_$name$args" << "{" << {
      ctx << "void pup(PUP::er &p)" << "{" << {
        "hypercomm::interpup(p, impl_);"
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
    val name = mailboxName(ctx, x)._1
    ctx << s"using tuple_t = typename decltype($name)::tuple_t;"
    ctx << s"auto __value__ = std::shared_ptr<tuple_t>(static_cast<tuple_t*>(malloc(sizeof(tuple_t))), [](tuple_t* x)" << "{"
    ctx << "x->~tuple_t();"
    ctx << "free(static_cast<void*>(x));" << "}" << ");"
    if (f.functionArgs.nonEmpty) {
      ctx << "ergoline::unpack(__msg__, *__value__);"
    }
    ctx << s"$name.put(std::move(__value__));"
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
      val isMailbox = x.isMailbox
      val args = f.functionArgs
      if (isMailbox) makeMailboxDecl(ctx, x)
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
      } else if (!isMailbox && (args.nonEmpty || isAsync)) {
        if (isAsync) {
          ctx << ctx.typeFor(f.returnType) << "__future__" << "("  << "PUP::reconstruct{})" << ";"
        }
        args.foreach(makeParameter(ctx, _))
        ctx << "ergoline::unpack(__msg__," << (Option.when(isAsync)("__future__") ++ args.map(_.name), ",") << ");"
      }
      if (isConstructor) {
        x.counterpart match {
          case Some(m) if m.isEntryOnly =>
            ctx << "this->impl_ = std::make_shared<" << base << ">(PUP::reconstruct{});"
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
