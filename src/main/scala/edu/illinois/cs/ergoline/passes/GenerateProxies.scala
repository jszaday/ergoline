package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCi.registerMailboxes
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp._
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

object GenerateProxies {

  val msgName = "__ckMsgPtr__"

  implicit val visitor: (CodeGenerationContext, EirNode) => Unit =
    (ctx, x) => GenerateCpp.visit(x)(ctx)

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

  def visitAbstractPup(
      ctx: CodeGenerationContext,
      indices: List[String]
  ): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{"
    ctx << "CProxyElement_ArrayBase::pup(p);"
    ctx << indices.map(idx => s"p | $idx;")
    ctx << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction): Unit = {
    val args = f.functionArgs
    val name = ctx.nameFor(f)
    // TODO support non-void returns?
    // TODO make a helper function that does a "send-thru" operation
    ctx << s"void $name(" << Option.when(args.nonEmpty)(
      s"CkMessage* $msgName"
    ) << ")" << "{" << {
      if (args.isEmpty) {
        ctx << "auto* " << msgName << "=" << "CkAllocateMarshallMsg(0, nullptr);"
      }

      ctx << "ckCheck();"
      ctx << "UsrToEnv(" << msgName << ")->setMsgtype(ForArrayEltMsg);"
      ctx << "CkArrayMessage* impl_amsg=(CkArrayMessage*)" << msgName << ";"
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

  def visitAbstractCons(
      ctx: CodeGenerationContext,
      base: EirProxy,
      derived: EirProxy
  ): Unit = {
    val tmp = "__derived__"
    ctx << ctx.nameFor(base) << "(" << "const" << ctx.nameFor(
      derived,
      Some(base)
    ) << "&" << tmp << ")"
    ctx << ":" << "CProxyElement_ArrayBase" << "(" << tmp << ".ckGetArrayID()," << tmp << ".ckGetIndex()" << ")" << "{"
    base.members
      .collect {
        case m @ EirMember(_, f: EirFunction, _) if m.isEntry =>
          derived.members.collectFirst {
            case n @ EirMember(_, g: EirFunction, _)
                if n.isEntry && (f.name == g.name) && CheckFunctions.sharedArgs(
                  ctx.typeContext,
                  f,
                  g
                ) => (f, m)
          }
      }
      .flatten
      .foreach { case (f, m) =>
        ctx << (ctx
          .nameFor(f) + "_idx__") << "=" << epIndexFor(derived, m)(ctx) << ";"
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
    impls
      .collect { case p: EirProxy => p }
      .foreach(visitAbstractCons(ctx, x, _))
    ctx << visitAbstractPup(ctx, indices) << {
      x.members.foreach(x =>
        visitAbstractEntry(ctx, assertValid[EirFunction](x.member))
      )
    } << "};" /* << {
      makeHasher(ctx, indices)
    } */
  }

  def indexForProxy(ctx: CodeGenerationContext, x: EirProxy): String = {
    x.indexType
      .map(ctx.typeFor(_))
      .getOrElse({
        assert(x.collective.isEmpty)

        "std::tuple<int,int>"
      })
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val mailboxes = x.mailboxes.map(mailboxName(ctx, _)._1).toList
    val base = ctx.nameFor(x.base)
    val name = x.baseName
    assert(name == s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}")
    GenerateCpp.visitTemplateArgs(x.templateArgs)(ctx)
    val args =
      if (x.templateArgs.nonEmpty) GenerateCpp.templateArgumentsToString(
        ctx,
        Some(x),
        x.templateArgs,
        Some(x)
      )
      else ""

    ctx << s"struct $name: public hypercomm::vil<CBase_$name$args" << "," << indexForProxy(
      ctx,
      x
    ) << ">" << "{"

    mailboxes.foreach(mboxName => {
      ctx << "static" << "int" << s"${mboxName}idx__" << ";"
    })

    val registerFn = "hypercomm::CkIndex_locality_base_::register_value_handler"
    ctx << s"static inline void $registerMailboxes(void)" << "{"
    mailboxes.foreach(mboxName => {
      ctx << s"${mboxName}idx__" << "=" << registerFn << "<" << s"${mboxName}fn__" << ">(\"" << s"$name::$mboxName" << "\");"
    })
    ctx << "}"

    // TODO call superclasses' PUP::er and Maiblox initer
    ctx << "inline void __init_mailboxes__(void) {"
    mailboxes.foreach(mboxName => {
      val ty = mboxName + "type"
      ctx << "using" << ty << "=" << "typename decltype(" << mboxName << ")::type;"
      ctx << "this->" << mboxName << s"=" << "this->emplace_component<" << ty << ">();"
    })
    ctx << "}"

    ctx << "void pup(PUP::er &p)" << "{" << {
      "hypercomm::interpup(p, impl_);"
    } << "}"

    ctx << {
      x.membersToGen
        .foreach {
          case m @ EirMember(_, f: EirFunction, _) if m.isMailbox =>
            visitMailbox(m, f)(ctx)
          case x => visitProxyMember(ctx, x)
        }
    } << "std::shared_ptr<" << nameFor(
      ctx,
      x.base,
      includeTemplates = true
    ) << ">" << s" impl_;" << s"};"
  }

  def makeArgsVector(
      ctx: CodeGenerationContext,
      name: String
  ): Unit = {
    val implMsg = "__ckArgsMsgPtr__"
    val argc = "__ckArgsShape__"
    assert(name != implMsg && name != argc)
    ctx << "auto*" << implMsg << "=" << s"(CkArgMsg*)$msgName" << ";"
    ctx << "std::array<std::size_t, 1>" << argc << s"= {(std::size_t) $implMsg->argc};"
    ctx << s"auto $name = std::make_shared<ergoline::array<std::string, 1>>(" << argc << ");"
    ctx << s"for (auto i = 0; i < $name->size(); i++) { new (&(*$name)[i]) std::string($implMsg->argv[i]); }"
  }

  def updateLocalityContext(implicit ctx: CodeGenerationContext): Unit = {
    // NOTE this is a temporary solution until an RTS-level one becomes available
    ctx << "this->update_context();"
  }

  private def makeEntryBody(
      ctx: CodeGenerationContext,
      member: EirMember
  ): Unit = {
    member.counterpart match {
      case Some(m: EirMember) if m.isMailbox => makeMailboxBody(ctx, member)
      case Some(m @ EirMember(_, f: EirFunction, _)) =>
        if (m.isEntryOnly) {
          ctx.pushSelf("this->impl_")
          ctx << "(([&](void) mutable" << visitFunctionBody(
            f
          )(ctx) << ")())"
          ctx.popSelf()
        } else {
          ctx << s"this->impl_->${ctx
            .nameFor(f)}(${f.functionArgs.map(ctx.nameFor(_)).mkString(", ")})"
        }
      case _ => Errors.unreachable()
    }
  }

  def visitMailbox(m: EirMember, f: EirFunction)(
      ctx: CodeGenerationContext
  ): Unit = {
    val (mboxName, tys) = mailboxName(ctx, m)
    val value = "val_"
    val implSelf = "impl_self_"
    val baseName = m.base.asInstanceOf[EirProxy].baseName
    ctx << "static" << "void" << s"${mboxName}fn__" << s"(hypercomm::generic_locality_* $implSelf, const hypercomm::entry_port_ptr&, hypercomm::value_ptr&& $value)" << "{"
    ctx << "auto*" << "self" << "=(" << baseName << s"*)$implSelf;"
    ctx << "self->" << mboxName << s"->receive_value(0, std::move($value));"
    ctx << "}"

    visitTemplateArgs(f)(ctx)
    makeMailboxDecl(ctx, m)
  }

  def mailboxName(
      ctx: CodeGenerationContext,
      node: EirNode,
      types: List[String]
  ): String = {
    // TODO impl this
    val prefix = ctx.nameFor(asMember(node).getOrElse(node))
    s"$prefix${if (prefix.endsWith("_")) "" else "_"}mailbox__"
  }

  def mailboxName(
      ctx: CodeGenerationContext,
      x: EirMember
  ): (String, List[String]) = {
    val f = assertValid[EirFunction](x.member)
    val tys = f.functionArgs.map(_.declaredType).map(ctx.typeFor(_, Some(x)))
    (mailboxName(ctx, x, tys), tys)
  }

  def makeMailboxDecl(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val (name, tys) = mailboxName(ctx, x)
    ctx << s"hypercomm::comproxy<ergoline::mailbox<${tys mkString ", "}>> $name;"
  }

  def getMailboxType(name: String): String =
    s"typename decltype($name)::type::type"

  def makeMailboxBody(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val mboxName = "this->" + mailboxName(ctx, x)._1
    val name = "__value__"
    val f = assertValid[EirFunction](x.member)
    val ty = name.init + "_type__"

    ctx << "using" << ty << "=" << getMailboxType(mboxName) << ";"
    ctx << "auto" << name << "="

    if (f.functionArgs.isEmpty) {
      ctx << "hypercomm::make_unit_value();"
    } else {
      ctx << "hypercomm::typed_value<" << ty << ">::from_message(" << msgName << ");"
    }

    ctx << mboxName << "->receive_value(0,std::move(" << name << "));"
  }

  def makeParameter(
      ctx: CodeGenerationContext,
      x: EirFunctionArgument
  ): Unit = {
    val ty = ctx.resolve(x.declaredType)
    val s = ctx.typeFor(x.declaredType, Some(x))
    if (ty.isPointer(ctx)) {
      ctx << s << x.name << ";"
    } else {
      ctx << "char" << s"__${x.name}__" << s"[sizeof($s)];"
      ctx << s"auto& ${x.name} = reinterpret_cast<$s&>(__${x.name}__);"
    }
  }

  def visitProxyMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val proxy = x.parent.to[EirProxy]
    val isConstructor = x.isConstructor
    val base = proxy
      .map(x => nameFor(ctx, x.base, includeTemplates = true))
      .getOrElse("")
    val f = assertValid[EirFunction](x.member)
    val isMain = proxy.exists(_.isMain)
    val isAsync = x.annotation("async").isDefined
    val isMailbox = x.isMailbox
    val args = f.functionArgs
    visitTemplateArgs(f)(ctx)
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
        ctx << (if (args.nonEmpty) s"CkMessage* $msgName" else "void")
      } else {
        ctx << Option.when(args.nonEmpty || isAsync)(s"CkMessage* $msgName")
      }
    } << ")" << "{"
    if (isMain && isConstructor) {
      args.headOption.foreach(x => makeArgsVector(ctx, x.name))
    } else if (!isMailbox && (args.nonEmpty || isAsync)) {
      if (isAsync) {
        ctx << ctx.typeFor(f.returnType) << "__future__" << ";"
      }
      args.foreach(makeParameter(ctx, _))
      ctx << s"hypercomm::unpack($msgName," << (Option.when(isAsync)(
        "__future__"
      ) ++ args.map(_.name), ",") << ");"
    }

    updateLocalityContext(ctx)

    if (isConstructor) {
      ctx << "this->__init_mailboxes__();"

      x.counterpart match {
        case Some(m) if m.isEntryOnly =>
          ctx << "this->impl_ = std::make_shared<" << base << ">(PUP::reconstruct{});"
          makeEntryBody(ctx, x)
          ctx << ";"
        case _ =>
          ctx << "this->impl_ = std::make_shared<" << base << ">(" << (args.map(
            ctx.nameFor(_)
          ), ", ") << ");"
      }
    } else {
      if (isAsync) {
        ctx << "__future__.set" << "(" << "hypercomm::pack_to_port({},"
      } else if (ctx.resolve(f.returnType) != globals.unitType) {
        ctx << "return "
      }
      makeEntryBody(ctx, x)
      if (isAsync) ctx << "));"
      else ctx << ";"
    }
    ctx << "}"
  }

}
