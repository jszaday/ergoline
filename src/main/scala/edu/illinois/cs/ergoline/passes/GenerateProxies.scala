package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp._
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

object GenerateProxies {

  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = (ctx, x) => GenerateCpp.visit(x)(ctx)

  def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val ns = x.namespaces.toList
    ctx << ns.map(ns => s"namespace ${ctx.nameFor(ns)} {") << {
      ctx.updateProxy(x)
      if (x.isAbstract) visitAbstractProxy(ctx, x)
      else {
//        if (x.collective.isEmpty) {
//          ctx << "/* readonly */ " << ("CProxy_" + x.baseName) << GenerateCi.readOnlyFor(x, None)(ctx) << ";"
//        }

        visitConcreteProxy(ctx, x)
      }
      ctx.updateProxy(x)
    } << ns.map(_ => "}")
  }

  def visitAbstractPup(ctx: CodeGenerationContext, indices: List[String]): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{"
    ctx << "CProxyElement_ArrayBase::pup(p);"
    ctx << indices.map(idx => s"p | $idx;")
    ctx << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction): Unit = {
    val args = f.functionArgs
    val name = ctx.nameFor(f)
    val msg = "__msg__"
    // TODO support non-void returns?
    // TODO make a helper function that does a "send-thru" operation
    ctx << s"void $name(" << Option.when(args.nonEmpty)(s"CkMessage* $msg") << ")" << "{" << {
      if (args.isEmpty) {
        ctx << "auto* " << msg << "=" << "CkAllocateMarshallMsg(0, nullptr);"
      }

      ctx << "ckCheck();"
      ctx << "UsrToEnv(" << msg << ")->setMsgtype(ForArrayEltMsg);"
      ctx << "CkArrayMessage* impl_amsg=(CkArrayMessage*)" << msg << ";"
      ctx << "impl_amsg->array_setIfNotThere(CkArray_IfNotThere_buffer);"
      ctx << "ckSend(impl_amsg," << (name + "_idx__") << ",0);"
    } << "}"
  }

  // TODO fixme
  def makeHasher(ctx: CodeGenerationContext, indices: List[String]): Unit = {
    ctx << "virtual hypercomm::hash_code hash(void) const override" << "{"
//    ctx << "ergoline::hasher _;"
//    ctx << "_" << "|" << "__id__" << ";"
//    ctx << "_" << "|" << "reinterpret_cast<std::underlying_type<CkEnvelopeType>::type&>(__msgType__)" << ";"
//    indices.foreach(x => {
//      ctx << "_" << "|" << x << ";"
//    })
    ctx << "return typeid(this).hash_code();"
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
    ctx << ":" << "CProxyElement_ArrayBase" << "(" << tmp << ".ckGetArrayID()," << tmp << ".ckGetIndex()" << ")" << "{"
    base.members.collect {
      case m@EirMember(_, f: EirFunction, _) if m.isEntry => derived.members.collectFirst {
        case n@EirMember(_, g: EirFunction, _) if n.isEntry && (f.name == g.name) && CheckFunctions.sharedArgs(ctx.typeContext, f, g) => (f, g)
      }
    }.flatten.foreach {
      case (f, g) => ctx << (ctx.nameFor(f) + "_idx__") << "=" << indexFor(ctx, derived, g) << ";"
    }
//    ctx << "__id__" << "=" << tmp << "." << "ckGetChareID()" << ";"
//    ctx << "__msgType__" << "=" << {
//      (derived.isElement, derived.collective) match {
//        case (false, None) => "ForChareMsg"
//        case (_, _) => ???
//      }
//    } << ";"
    ctx << "}"
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = ctx.nameFor(x)
    val impls = x.derived.toList
    val indices = x.members.map(m => ctx.nameFor(m) + "_idx__")
    ctx << s"struct $name: public CProxyElement_ArrayBase" << "{"
    // TODO public ergoline::hashable,
//    ctx << "CkChareID" << "__id__" << ";"
//    ctx << "CkEnvelopeType" << "__msgType__" << ";"
    indices.foreach(idx => ctx << "int" << idx << ";")
    ctx << name << "(" /* << TODO "PUP::reconstruct" */ << ")" << "{}"
    impls.collect { case p: EirProxy => p }.foreach(visitAbstractCons(ctx, x, _))
    ctx << visitAbstractPup(ctx, indices) << {
      x.members.foreach(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member)))
    } << "};" /* << {
      makeHasher(ctx, indices)
    } */
  }

  def indexForProxy(x: EirProxy): String = {
    ProxyManager.dimensionality(x.collective.getOrElse("array2d")) match {
      case 1 => "int"
      case n => "std::tuple<" + ((0 until n).map(_ => "int") mkString ",") + ">"
    }
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val base = ctx.nameFor(x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    GenerateCpp.visitTemplateArgs(x.templateArgs)(ctx)
    val args = if (x.templateArgs.nonEmpty) GenerateCpp.templateArgumentsToString(ctx, x.templateArgs, None) else ""
    val mailboxes = x.members.filter(_.isMailbox).map(mailboxName(ctx, _)._1)

    ctx << s"struct $name: public hypercomm::vil<CBase_$name$args" << "," << indexForProxy(x) << ">" << "{"

    // TODO call superclasses' PUP::er and Maiblox initer
    ctx << "inline void __init_mailboxes__(void) {"
    mailboxes.foreach(mboxName => {
      val ty = mboxName + "type"
      ctx << "using" << ty << "=" << "typename decltype(" << mboxName << ")::element_type;"
      ctx << "this->" << mboxName << s"=std::dynamic_pointer_cast<$ty>(" << "this->emplace_component<" << ty << ">());"
    })
    ctx << "}"

    ctx << "void pup(PUP::er &p)" << "{" << {
      "hypercomm::interpup(p, impl_);"
    } << "}"

    ctx << {
      x.membersToGen
        .foreach(x => visitProxyMember(ctx, x))
    } << "std::shared_ptr<" << nameFor(ctx, x.base, includeTemplates = true) << ">" << s" impl_;" << s"};"
  }

  def makeArgsVector(ctx: CodeGenerationContext, name: String): Unit = {
    assert(name != "msg" && name != "_argc_")
    ctx << "auto" << "msg" << "=" << s"(CkArgMsg*)__msg_tmp__" << ";"
    ctx << "std::array<std::size_t, 1> _argc_ = {(std::size_t) msg->argc};"
    ctx << s"auto $name = std::make_shared<ergoline::array<std::string, 1>>(_argc_);"
    ctx << s"for (auto i = 0; i < args->size(); i++) { new (&(*$name)[i]) std::string(msg->argv[i]); }"
  }

  def updateLocalityContext(implicit ctx: CodeGenerationContext): Unit = {
    // NOTE this is a temporary solution until an RTS-level one becomes available
    ctx << "this->update_context();"
  }

  private def makeEntryBody(ctx: CodeGenerationContext, member: EirMember): Unit = {
    updateLocalityContext(ctx)

    member.counterpart match {
      case Some(m: EirMember) if m.isMailbox => makeMailboxBody(ctx, member)
      case Some(m@EirMember(_, f: EirFunction, _)) =>
        if (m.isEntryOnly) {
          ctx << "(([&](void) mutable" << visitFunctionBody(f)(ctx) << ")())"
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
    ctx << s"std::shared_ptr<ergoline::mailbox<${tys mkString ", "}>> $name;"
  }

  def getMailboxType(name: String): String =
    s"typename decltype($name)::element_type::type"

  def makeMailboxBody(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val mboxName = "this->" + mailboxName(ctx, x)._1
    val name = "__value__"
    val f = assertValid[EirFunction](x.member)
    val msg = if (f.functionArgs.isEmpty) "nullptr" else "__msg__"
    val ty = name.init + "_type__"

    ctx << "using" << ty << "=" << getMailboxType(mboxName) << ";"
    ctx << "auto" << name << "=" << "std::make_shared<hypercomm::typed_value<" << ty << ">>(" << msg << ");"
    ctx << mboxName << "->receive_value(0,std::move(" << name << "));"
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
          ctx << (if (args.nonEmpty) "CkMessage* __msg_tmp__" else "void")
        } else {
          ctx << Option.when(args.nonEmpty || isAsync)("CkMessage* __msg__")
        }
      } << ")" << "{"
      if (isMain && isConstructor) {
        args.headOption.foreach(x => makeArgsVector(ctx, x.name))
      } else if (!isMailbox && (args.nonEmpty || isAsync)) {
        if (isAsync) {
          ctx << ctx.typeFor(f.returnType) << "__future__" << ";"
        }
        args.foreach(makeParameter(ctx, _))
        ctx << "hypercomm::unpack(__msg__," << (Option.when(isAsync)("__future__") ++ args.map(_.name), ",") << ");"
      }
      if (isConstructor) {
        ctx << "this->__init_mailboxes__();"

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
          ctx << "ergoline::send_future" << "(" << "__future__" << "," << "ergoline::pack" << "("
        } else if (ctx.resolve(f.returnType) != globals.typeFor(EirLiteralTypes.Unit)) {
          ctx << "return "
        }
        makeEntryBody(ctx, x)
        if (isAsync) ctx << "));"
        else ctx << ";"
      }
      ctx << "}"
  }

}
