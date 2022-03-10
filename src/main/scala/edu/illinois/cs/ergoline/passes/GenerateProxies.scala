package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCi.registerMailboxes
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp._
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

object GenerateProxies {

  val msgName = "__ckMsgPtr__"

  def indicesName(proxy: EirProxy): String = s"__${proxy.baseName}indices__"
  def makeIndices(ctx: CodeGenerationContext, proxy: EirProxy): Unit = {
    val mbs = proxy.mailboxes.map(mailboxName(ctx, _)._1).toList
    if (mbs.nonEmpty) {
      val idxName = indicesName(proxy)
      val ns = proxy.namespaces.toList
      ns.foreach(ctx << "namespace" << ctx.nameFor(_) << "{")
      ctx << "struct" << idxName << "{"
      mbs.foreach(name => {
        ctx << "static" << "int" << s"${name}idx__" << ";"
      })
      ctx << "};"
      mbs.foreach(name => {
        ctx << "int" << s"$idxName::${name}idx__" << ";"
      })
      ns.foreach(_ => ctx << "}")
    }
  }

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

  private val ckArrayBase = "CProxyElement_ArrayElement"

  def visitAbstractPup(
      ctx: CodeGenerationContext,
      indices: List[String]
  ): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{"
    ctx << ckArrayBase << "::pup(p);"
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
      ctx << "ckSend(impl_amsg," << abstractIndexOf(name) << ",0);"
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
    ctx << ":" << ckArrayBase << "(" << tmp << ".ckGetArrayID()," << tmp << ".ckGetIndex()" << ")" << "{"
    base.members
      .collect {
        case m @ EirMember(_, f: EirFunction, _) if m.isEntry =>
          derived.members.collectFirst {
            case n @ EirMember(_, g: EirFunction, _)
                if n.isEntry && (f.name == g.name) && CheckFunctions.sharedArgs(
                  ctx.typeContext,
                  f,
                  g
                ) => (f, n)
          }
      }
      .flatten
      .foreach { case (f, n) =>
        ctx << abstractIndexOf(ctx.nameFor(f)) << "=" << epIndexFor(
          derived,
          n,
          Some(base),
          None
        )(
          ctx
        ) << ";"
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

  private def abstractIndexOf(name: String): String = {
    name.substring(0, name.length - 1) + "idx__"
  }

  def abstractIndexOf(
      entry: EirMember
  )(implicit ctx: CodeGenerationContext): String = {
    abstractIndexOf(ctx.nameFor(entry))
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = ctx.nameFor(x)
    val impls = x.derived.toList
    val indices = x.members.map(abstractIndexOf(_)(ctx))
    ctx << s"struct $name: public" << ckArrayBase << "{"
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

  def formatArgs(ctx: CodeGenerationContext, entry: EirMember): List[String] = {
    val fn = assertValid[EirFunction](entry.member)
    fn.functionArgs.map(_.declaredType).map(ctx.typeFor(_, Some(entry)))
  }

  val valueSuffix = "value"
  val deliverableSuffix = "deliverable"

  def valueHandlerFor(
      ctx: CodeGenerationContext,
      member: EirMember,
      suffix: String
  ): String = {
    val name = ctx.nameFor(member, None)
    s"${name.substring(0, name.length - 1)}${suffix}_handler__"
  }

  private def containTypes(tys: List[String]): String = {
    if (tys.length == 1) tys.head else s"std::tuple<${tys mkString ","}>"
  }

  def generateHandlers(ctx: CodeGenerationContext, entry: EirMember): Unit = {
    val fn = assertValid[EirFunction](entry.member)
    val retTy = Find.uniqueResolution[EirType](fn.returnType)
    val unitRet = retTy == globals.unitType
    val ts = fn.templateArgs
    val baseName = assertValid[EirProxy](entry.base).baseName
    val tys = formatArgs(ctx, entry)
    val valHandler = valueHandlerFor(ctx, entry, valueSuffix)
    val isAsync = entry.annotation("async").nonEmpty
    val threaded = entry.annotation("threaded").nonEmpty
    val future = Option.when(isAsync)("fut")
    val args = () => {
      if (ts.nonEmpty) ctx << "<" << (ts.map(_.name), ",") << ">"
      else ctx
    }
    if (!unitRet && threaded) {
      Errors.unreachable()
    }
    visitTemplateArgs(ts)(ctx)
    ctx << "static" << (if (isAsync) "void"
                        else
                          ctx.typeFor(retTy, Some(entry))) << valueHandlerFor(
      ctx,
      entry,
      deliverableSuffix
    ) << "(hypercomm::generic_locality_* self, hypercomm::deliverable&& dev)" << "{"
    if (isAsync) {
      ctx << "hypercomm::future" << future << ";"
      ctx << "auto val = hypercomm::make_typed_value<" << containTypes(
        tys
      ) << ">(hypercomm::tags::no_init());"
      ctx << "hypercomm::unpack(dev.release<CkMessage>(), " << future << ", val->value());"
    } else {
      ctx << "auto val = hypercomm::dev2typed<" << containTypes(
        tys
      ) << ">(std::move(dev));"
    }
    if (threaded) {
      ctx << "CthThread tid = CthCreate((CthVoidFn)" << valHandler << args() << s", new CkThrCallArg(val.release(), self), ${globals.defaultStackSize});"
      ctx << "self->CkAddThreadListeners(tid, nullptr); // TODO (this will fail if CMK_TRACE_ENABLED) ;"
      ctx << "CthTraceResume(tid);"
      ctx << "CthResume(tid);"
    } else {
      if (!(isAsync || unitRet)) {
        ctx << "return ("
      }
      ctx << "((" << baseName << "*)self)->" << valHandler << args() << "(" << future
        .map(_ + ",") << "std::move(val))"
      if (isAsync || unitRet) ctx << ";"
      else ctx << ");"
    }
    ctx << "}"
    makeEntry(ctx, entry, valHandler, Some(tys))
  }

  val localityPrefix = "hypercomm::CkIndex_locality_base_::"

  def makeHandler(
      ctx: CodeGenerationContext,
      ty: EirType,
      entry: EirMember
  ): Unit = {
    val fn = entry.counterpart.map(_.member).to[EirFunction]
    val handlerFn = s"${localityPrefix}put_value_handler"
    val helper = (sp: EirSpecialization) => {
      val osp = Option(sp)
      ctx << handlerFn << "(" << GenerateCpp.epIndexFor(
        ty,
        entry,
        None,
        hasArgs = true,
        osp
      )(ctx) << "," << valueHandlerFor(ctx, entry, deliverableSuffix)
      osp.foreach(_ =>
        ctx << templateArgumentsToString(sp.types, Some(entry))(ctx)
      )
      ctx << ");"
    }

    fn match {
      case Some(x) if x.templateArgs.nonEmpty => ctx.checked(x).foreach(helper)
      case _                                  => helper(null)
    }
  }

  def makeMailboxInit(
      ctx: CodeGenerationContext,
      mailboxNames: List[String]
  ): Unit = {
    // TODO call superclasses' PUP::er and Maiblox initer
    ctx << "inline void __init_mailboxes__(void) {"
    mailboxNames.foreach(mboxName => {
      val ty = mboxName + "type"
      ctx << "using" << ty << "=" << "typename decltype(" << mboxName << ")::type;"
      ctx << "this->" << mboxName << s"=" << "this->emplace_component<" << ty << ">();"
    })
    ctx << "}"
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val members = x.members
    val entries = members
      .filter(x => x.isEntry && !(x.isConstructor || x.isMailbox))
      .filter(GenerateCpp.memberHasArgs)
    val mailboxes = members.filter(_.isMailbox)
    val mailboxNames = mailboxes.map(mailboxName(ctx, _)._1)
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

    mailboxes.foreach(makeMailboxDecl(ctx, _))
    makeMailboxInit(ctx, mailboxNames)

    val thisType = Find.uniqueResolution[EirType](x.asType)
    val idxName = indicesName(x)
    val registerFn = s"${localityPrefix}register_value_handler"
    ctx << s"static inline void $registerMailboxes(void)" << "{"
    mailboxNames.foreach(mboxName => {
      ctx << s"$idxName::${mboxName}idx__" << "=" << registerFn << "<" << s"${mboxName}fn__" << ">(\"" << s"$name::$mboxName" << "\");"
    })
    entries
      // local EPs can't have value handlers
      .filterNot(_.isLocal)
      .foreach(makeHandler(ctx, thisType, _))
    ctx << "}"

    entries.foreach(generateHandlers(ctx, _))

    ctx << "void pup(PUP::er &p)" << "{" << {
      "hypercomm::interpup(p, impl_);"
    } << "}"

    ctx << {
      x.membersToGen
        .foreach {
          case m @ EirMember(_, f: EirFunction, _) if m.isMailbox =>
            visitMailbox(m, f)(ctx)
          case EirMember(_, _: EirTypeAlias, _) =>
          // TODO ( should we actually generate something here? )
          case y => visitProxyMember(ctx, x, y)
        }
    } << "std::shared_ptr<" << nameFor(
      ctx,
      x.base,
      includeTemplates = true
    ) << ">" << s" impl_;" << s"};"

    if (x.templateArgs.nonEmpty) {
      ctx << "void" << GenerateCi.makeGenericRegistration(x) << "(void)" << "{"
      val specs = ctx.checked
        .getOrElse(x.base, Nil)
        .map(
          _.types.map(Find.uniqueResolution[EirType])
        )
      specs.foreach(sp => {
        ctx << x.baseName << "<" << {
          (sp.map(ctx.typeFor(_, Some(x))), ", ")
        } << ">::" << registerMailboxes << "()" << ";"
      })
      ctx << "}"
    }
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
    ctx << s"auto $name = ergoline::array<std::string, 1>::instantiate(" << argc << ");"
    ctx << s"for (auto i = 0; i < $name->size(); i++) { new (&(*$name)[i]) std::string($implMsg->argv[i]); }"
  }

  def updateLocalityContext(implicit ctx: CodeGenerationContext): Unit = {
    // NOTE this is a temporary solution until an RTS-level one becomes available
    ctx << ctx.currentProxySelf << "->update_context();"
  }

  private def makeEntryBody(
      ctx: CodeGenerationContext,
      member: EirMember,
      msgName: Option[String]
  ): Unit = {
    (msgName, member.counterpart) match {
      case (Some(name), _) if !member.isConstructor =>
        val fn = assertValid[EirFunction](member.member)
        val ts = fn.templateArgs
        ctx << valueHandlerFor(
          ctx,
          member,
          deliverableSuffix
        )
        if (ts.nonEmpty) {
          ctx << "<" << (ts.map(_.name), ",") << ">"
        }
        ctx << s"(${ctx.currentProxySelf}, hypercomm::deliverable($name))"
      case (_, Some(m: EirMember)) if m.isMailbox => Errors.unreachable()
      case (_, Some(m @ EirMember(_, f: EirFunction, _))) =>
        if (m.isEntryOnly) {
          ctx.pushSelf(ctx.currentProxySelf + "->impl_")
          ctx << "(([&](void) mutable" << makeMemberBody(
            ctx,
            member,
            f
          ) << ")())"
          ctx.popSelf()
        } else {
          ctx << ctx.currentProxySelf << "->impl_->" << ctx.nameFor(
            f
          ) << s"(${f.functionArgs.map(ctx.nameFor(_)).mkString(", ")})"
        }
      case _ => Errors.unreachable()
    }
  }

  def visitMailbox(m: EirMember, f: EirFunction)(
      ctx: CodeGenerationContext
  ): Unit = {
    val (mboxName, _) = mailboxName(ctx, m)
    val value = "dev_"
    val implSelf = "impl_self_"
    val baseName = m.base.asInstanceOf[EirProxy].baseName
    ctx << "static" << "void" << s"${mboxName}fn__" << s"(hypercomm::generic_locality_* $implSelf, hypercomm::deliverable&& $value)" << "{"
    ctx << "auto*" << "self" << "=(" << baseName << s"*)$implSelf;"
    ctx << "auto status = self->passthru(std::make_pair((hypercomm::component_id_t)self->" << mboxName << ", 0), " << value << ");"
    ctx << "CkAssert(status);"
    ctx << "}"
    visitTemplateArgs(f)(ctx)
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

  // TODO ( rename this to "mailboxInfo" )
  def mailboxName(
      ctx: CodeGenerationContext,
      x: EirMember
  ): (String, List[String]) = {
    val tys = formatArgs(ctx, x)
    (mailboxName(ctx, x, tys), tys)
  }

  def mailboxType(name: String): String = s"${name}type__"

  def makeMailboxDecl(ctx: CodeGenerationContext, x: EirMember): Unit = {
    val (name, tys) = mailboxName(ctx, x)
    ctx << s"using ${mailboxType(name)} = ergoline::mailbox<${tys mkString ", "}>;"
    ctx << s"hypercomm::comproxy<${name}type__> $name;"
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

  def proxyMemberSelf(proxyType: String): String = "thisProxy"

  def proxyMemberProxySelf: String =
    s"((hypercomm::generic_locality_*)${globals.implicitProxyName}->local())"

  def makeMemberBody(
      ctx: CodeGenerationContext,
      member: EirMember,
      fn: EirFunction
  ): Unit = {
    if (
      !member.hasAnnotation("threaded") || !GenerateSdag.visit(member, fn)(ctx)
    ) {
      visitFunctionBody(fn)(ctx)
    }
  }

  def makeProxyMember(
      ctx: CodeGenerationContext,
      proxy: EirProxy,
      member: EirMember
  ): Unit = {
    val fn = assertValid[EirFunction](member.member)
    val args = fn.functionArgs ++ fn.implicitArgs
    val proxyType = ctx.typeFor(proxy.asType)
    ctx << "static" << ctx.typeFor(fn.returnType, Some(fn)) << ctx.nameFor(
      member
    ) << "(" << "const" << proxyType << "&" << "thisProxy"
    ctx << Option.unless(args.isEmpty)(",") << (args, ",")
    ctx << ")" << "{"
    ctx.pushSelf(proxyMemberSelf(proxyType))
    ctx.pushProxySelf(proxyMemberProxySelf)
    makeMemberBody(ctx, member, fn)
    ctx.popProxySelf()
    ctx.popSelf()
    ctx << "}"
  }

  def visitProxyMember(
      ctx: CodeGenerationContext,
      proxy: EirProxy,
      x: EirMember
  ): Unit = {
    val isConstructor = x.isConstructor
    val f = assertValid[EirFunction](x.member)
    if (x.annotation("proxy").nonEmpty) {
      makeProxyMember(ctx, proxy, x)
    } else {
      makeEntry(
        ctx,
        x, {
          if (isConstructor) {
            val baseName = ctx.nameFor(proxy.base)
            s"${baseName}_${proxy.collective.map(x => s"${x}_").getOrElse("")}"
          } else {
            ctx.nameFor(f)
          }
        },
        None
      )

      val lines = GenerateSdag.generated.get(x).toList.flatMap(_.lines)
      ctx << lines
      if (lines.nonEmpty) {
        GenerateSdag.generated.remove(x)
      }
    }
  }

  val threadedSelf = "self"
  val asyncFuture = "__future__"

  def makeEntry(
      ctx: CodeGenerationContext,
      x: EirMember,
      name: String,
      tys: Option[List[String]]
  ): Unit = {
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
    assert(f.implicitArgs.isEmpty)
    val valName = tys.map(_ => "val")
    val msgName = Option.when(tys.isEmpty)(GenerateProxies.msgName)
    val hasArgs = args.nonEmpty || isAsync
    val threaded = x.annotation("threaded").nonEmpty
    val threadedHandler = valName.exists(_ => threaded)
    val future = Option.when(isAsync)(asyncFuture)
    val threadArg = Option.when(threadedHandler)("__ckThrCallArg__")
    val asyncHandler = valName.nonEmpty || args.isEmpty
    visitTemplateArgs(f)(ctx)

    if (threadedHandler) {
      ctx << "static"
    }

    if (!isConstructor) {
      ctx << (if (isAsync) "void" else ctx.typeFor(f.returnType))
    }
    ctx << name << "("

    tys match {
      case Some(x) =>
        future.foreach(f => ctx << "hypercomm::future&" << f << ",")
        if (threadedHandler) {
          ctx << "CkThrCallArg*" << threadArg
        } else {
          ctx << s"hypercomm::typed_value_ptr<${containTypes(x)}>&&" << valName
        }
      case None => if (hasArgs) {
          ctx << "CkMessage*" << msgName
        }
    }
    ctx << ")" << "{"

    future
      .zip(msgName)
      .filter(_ => args.isEmpty)
      .foreach { case (f, m) =>
        ctx << "hypercomm::future" << f << ";"
        ctx << "hypercomm::unpack(" << m << "," << f << ");"
      }

    threadArg.foreach(arg => {
      val contained = containTypes(tys.get)
      ctx << "hypercomm::typed_value_ptr<" << contained << "> val((hypercomm::typed_value<" << contained << ">*)" << arg << "->msg);"
      ctx << "auto*" << threadedSelf << "=" << "(" << proxy.map(
        _.baseName
      ) << "*)" << arg << "->obj;"
      ctx << "delete" << arg << ";"
      ctx.pushProxySelf(threadedSelf)
    })

    if (isMain && isConstructor) {
      args.headOption.foreach(x => makeArgsVector(ctx, x.name))
    } else if (!isMailbox && hasArgs) {
      if (isConstructor) {
        args.foreach(makeParameter(ctx, _))
        ctx << s"hypercomm::unpack(" << msgName << "," << (args.map(
          _.name
        ), ",") << ");"
      } else {
        tys match {
          case Some(x) if x.length == 1 =>
            ctx << "auto&" << args.headOption.map(
              _.name
            ) << "=" << valName << "->value();"
          case Some(_) => args.zipWithIndex.foreach { case (x, i) =>
              ctx << s"auto& ${x.name} = std::get<$i>(" << valName << "->value());"
            }
          case _ =>
//        TODO ( this code-path does not work well for async )
//        if (isAsync) {
//          ctx << ctx.typeFor(f.returnType) << "__future__" << ";"
//        (Option.when(isAsync)("__future__")
//        }
        }
      }
    }

    if (isConstructor) {
      updateLocalityContext(ctx)

      ctx << "this->__init_mailboxes__();"

      x.counterpart match {
        case Some(m) if m.isEntryOnly =>
          ctx << "this->impl_ = std::make_shared<" << base << ">(PUP::reconstruct{});"
          makeEntryBody(ctx, x, msgName)
          ctx << ";"
        case _ =>
          ctx << "this->impl_ = std::make_shared<" << base << ">(" << (args.map(
            ctx.nameFor(_)
          ), ", ") << ");"
      }
    } else {
      if (valName.nonEmpty || !hasArgs) {
        updateLocalityContext(ctx)
      }
      if (isAsync && asyncHandler) {
        ctx << s"$asyncFuture.set" << "(" << "hypercomm::pack_to_port({},"
      } else if (!(isAsync || ctx.resolve(f.returnType) == globals.unitType)) {
        ctx << "return "
      }
      makeEntryBody(ctx, x, msgName.filterNot(_ => args.isEmpty))
      if (isAsync && asyncHandler) ctx << "));"
      else ctx << ";"
    }

    threadArg.foreach(_ => ctx.popProxySelf())

    ctx << "}"
  }

}
