package edu.illinois.cs.ergoline.passes

import java.io.File
import java.nio.file.Paths
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{
  EirElementProxy,
  EirLambdaType,
  EirSectionProxy,
  EirTemplatedType,
  EirTupleType,
  EirType
}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateProxies.{
  getMailboxType,
  updateLocalityContext
}
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends EirVisitor[CodeGenerationContext, Unit] {
  var visited: List[EirNode] = Nil
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = (ctx, x) =>
    visit(x)(ctx)

  object GenCppSyntax {
    implicit class RichEirNode(self: EirNode) {
      def isSystem: Boolean =
        self.parent match {
          case Some(m: EirMember) => m.isSystem
          case _                  => self.annotation("system").isDefined
        }
    }

    implicit class RichEirType(self: EirType) {
      def isReconstructible: Boolean = {
        // TODO refine this with @system(reconstructible=true)
        // TODO eliminate the check for EirTemplateArgument here
        // TODO fix reconstruction of tuple types!
        self match {
          case _: EirTemplateArgument | _: EirTupleType          => false
          case _: EirProxy | EirTemplatedType(_, _: EirProxy, _) => false
          case _                                                 => !self.isPointer && !self.isSystem
        }
      }

      def isPointer: Boolean = {
        self.isInstanceOf[EirLambdaType] || Find
          .tryClassLike(self)
          .exists(!_.isValueType)
      }

      def isSystem: Boolean =
        Find.asClassLike(self).annotation("system").isDefined

      def isTrait: Boolean =
        Find.asClassLike(self).isInstanceOf[EirTrait]
    }

    implicit class RichEirResolvable[T <: EirNode](self: EirResolvable[T]) {
      def isTransient: Boolean = {
        val x = Find.uniqueResolution[EirNode](self)
        x match {
          case t: EirTupleType => t.children.exists(_.isTransient)
          // TODO use specialization?
          case _: EirTemplateArgument => false
          case _: EirLambdaType       => false
          case _: EirClassLike =>
            x.parent.exists(_.isInstanceOf[EirMember]) || x
              .annotation("transient")
              .isDefined
          case t: EirType => Find.asClassLike(t).isTransient
        }
      }
    }
  }

  import GenCppSyntax.{RichEirNode, RichEirResolvable, RichEirType}

  def forwardDecl(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    visitTemplateArgs(x.templateArgs)
    ctx << s"struct ${ctx.nameFor(x)};"

    ProxyManager
      .proxiesFor(x)
      .foreach(p => {
        visitTemplateArgs(p.templateArgs)
        ctx << s"struct ${ctx.nameFor(p)};"
      })
  }

//  override def visitNamespace(node: EirNamespace)(implicit ctx: CodeGenerationContext): Unit = {
//    val parted = node.children.partition(_.isInstanceOf[EirClassLike])
//    node.children = parted._1.map(_.asInstanceOf[EirClassLike]).sorted ++ parted._2
//    super.visitNamespace(ctx, node)
//  }

  def zipWithSpecializations[A <: EirSpecializable](
      as: Iterable[A]
  )(implicit
      ctx: CodeGenerationContext
  ): List[(A, List[EirResolvable[EirType]])] = {
    as.flatMap(a => {
      if (a.templateArgs.isEmpty) Seq((a, Nil))
      else ctx.checked(a).map(s => (a, s.types))
    }).toList
  }

  def declareGlobals(implicit ctx: CodeGenerationContext): Unit = {
    val ns = Some(EirGlobalNamespace)

    zipWithSpecializations(
      ProxyManager.singletons
        .filterNot(_.isAbstract)
    ) foreach {
      case (p, types) =>
        val (ty, name) = ("int", counterFor(p, types, ns))
        ctx << "CpvDeclare(" << ty << "," << name << ");"

        val namespaces =
          Find.ancestors(p).collect { case n: EirNamespace => n }.toList
        namespaces.reverse.foreach(ns => ctx << "namespace" << ns.name << "{")
        ctx << "/* readonly */ " << collectiveTypeFor(
          p,
          types
        ) << " " << readOnlyFor(p, types, None)(ctx) << ";"
        namespaces.foreach(_ => ctx << "}")
    }
  }

  def generateMain(implicit ctx: CodeGenerationContext): Unit = {
    val ns = Some(EirGlobalNamespace)
    GenerateCi.generatedMain match {
      case Some(mainName) =>
        val msgName = "__msg__"
        ctx << "struct" << mainName << ":" << "public" << ("CBase_" + mainName) << "{"
        ctx << mainName << "(" << "CkArgMsg*" << msgName << ")" << "{"
        val concreteSingletons =
          zipWithSpecializations(
            ProxyManager.singletons.filterNot(_.isAbstract)
          )

        concreteSingletons foreach {
          case (p, types) =>
            ctx << readOnlyFor(p, types, ns) << "=" << {
              (qualificationsFor(p, ns.get) :+ collectiveTypeFor(p, types))
                .mkString("::")
            } << "::ckNew();"
        }

        concreteSingletons collect {
          case (p, _) if p.isMain => assert(p.templateArgs.isEmpty); p
        } foreach (p => {
          ctx << "ergoline::create_element(" << readOnlyFor(
            p,
            Nil,
            ns
          ) << "," << {
            indexForSingleton(p, Nil)
          } << "," << {
            if (
              !p.members.filter(_.isConstructor).exists {
                case EirMember(_, f: EirFunction, _) => f.functionArgs.isEmpty
                case _                               => false
              }
            ) {
              ctx << "(CkMessage*)CkCopyMsg((void**)&" << msgName << ")" << ","
            }
            ctx << "CkMyPe()"
          } << ");"
        })

        ctx << "// CkFreeMsg(" << msgName << ");"
        ctx << "}" << "};"
      case None =>
    }
  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    x.namespaces.foreach(ns =>
      ctx << s"namespace" << ctx.nameFor(ns) << "{" << "struct" << ctx.nameFor(
        x
      ) << ";" << "}"
    )
  }

  def mkTypeSuffix(
      types: List[EirResolvable[EirType]],
      occurrence: Option[EirNode]
  )(implicit
      ctx: CodeGenerationContext
  ): String = {
    types
      .map(ctx.typeFor(_, occurrence))
      .mkString("_") + Option.when(types.nonEmpty)("_").getOrElse("")
  }

  // TODO this may need a context/occurrence?
  def collectiveTypeFor(p: EirProxy, types: List[EirResolvable[EirType]])(
      implicit ctx: CodeGenerationContext
  ): String = {
    "CProxy_" + p.baseName + {
      if (types.nonEmpty) {
        "<" + (types.map(ctx.typeFor(_)) mkString ",") + ">"
      } else {
        ""
      }
    }
  }

  def counterFor(
      p: EirProxy,
      types: List[EirResolvable[EirType]],
      occurrence: Option[EirNode] = Some(EirGlobalNamespace)
  )(implicit ctx: CodeGenerationContext): String = {
    "__" + {
      ctx
        .nameFor(p, occurrence)
        .replaceAll("CProxy_", "")
        .replaceAll("::", "_")
    } + {
      mkTypeSuffix(types, occurrence)
    } + "ctr__"
  }

  def readOnlyFor(
      p: EirProxy,
      types: List[EirResolvable[EirType]],
      occurrence: Option[EirNode]
  )(implicit
      ctx: CodeGenerationContext
  ): String = {
    ctx.nameFor(p, occurrence) + {
      mkTypeSuffix(types, occurrence)
    } + "ro__"
  }

  def indexForSingleton(p: EirProxy, types: List[EirResolvable[EirType]])(
      implicit ctx: CodeGenerationContext
  ): Unit = {
    ctx << "std::make_tuple("
    ctx << "CkMyPe()" << ","
    ctx << "++CpvAccess(" << counterFor(p, types)(ctx) << ")"
    ctx << ")"
  }

  val corePupables: Seq[String] = Seq(
    "hypercomm::future_port",
    "hypercomm::port_opener",
    "hypercomm::forwarding_callback",
    "hypercomm::inter_callback"
  )

  val localityPupables: Seq[String] = Seq(
    "hypercomm::vector_section",
    "hypercomm::reduction_port",
    "hypercomm::broadcaster"
  )

  def registerPolymorphs(ctx: CodeGenerationContext): Unit = {
    val global = Some(EirGlobalNamespace)
    val checked = ctx.checked
    val puppables = checked.keys.filter({
      case _: EirProxy => false
      case x: EirClassLike =>
        !(x.annotation("system").isDefined || x.isAbstract || x.isTransient)
      case _ => false
    })

    ctx << "void setup_environment(void)" << "{"

    ctx << "hypercomm::init_polymorph_registry();"
    ctx << "if(CkMyRank()==0)" << "{"

    ProxyManager.registeredIndices() foreach { x =>
      localityPupables foreach (y => {
        ctx << "hypercomm::enroll<" << y
        ctx << "<" << ctx.typeFor(x, global) << ">>" << "()" << ";"
      })
    }

    puppables.foreach(x => {
      if (x.templateArgs.isEmpty) {
        ctx << "hypercomm::enroll<" << ctx
          .nameFor(x, global) << ">" << "()" << ";"
      } else {
        checked(x).foreach(y => {
          ctx.specialize(x, y)
          ctx << "hypercomm::enroll<" << ctx.nameFor(
            x,
            global
          ) << ">" << "()" << ";"
          ctx.leave(y)
        })
      }
    })

    ctx.lambdas
      .flatMap(_._2)
      .foreach(x => {
        ctx << "hypercomm::enroll<" << ctx
          .nameFor(x, global) << ">" << "()" << ";"
      })

    corePupables.foreach(x => {
      ctx << "hypercomm::enroll<" << x << ">" << "()" << ";"
    })

    ctx << "}"

    zipWithSpecializations(
      ProxyManager.singletons
        .filterNot(_.isAbstract)
    )(ctx) foreach {
      case (p, types) =>
        val (ty, name) = ("int", counterFor(p, types, global)(ctx))
        ctx << "CpvInitialize(" << ty << "," << name << ");"
        ctx << "CpvAccess(" << name << ") = 0;"
    }

    ctx << "}"
  }

  override def error(node: EirNode)(implicit ctx: CodeGenerationContext): Unit =
    ()

  private def arrayMember(
      ctx: CodeGenerationContext,
      x: Option[EirNode]
  ): Option[String] = {
    asMember(x).collect {
      case m: EirMember if isArray(ctx, m.base) => m.name
    }
  }

  override def visitScopedSymbol[A <: EirNode](
      x: EirScopedSymbol[A]
  )(implicit ctx: CodeGenerationContext): Unit = {
    // TODO handle self applications :3
    val found = disambiguate(ctx, x)
    arrayMember(ctx, Some(found)) match {
      case Some("size") =>
        arrayDim(ctx, ctx.typeOf(x.target)) match {
          case Some(1) => ctx << x.target << "->shape[0]"
          case Some(n) if n > 0 =>
            ctx << "std::tuple_cat(" << x.target << "->shape)"
          case _ => Errors.unreachable()
        }
      case _ =>
        val targetTy: EirType = ctx.exprType(x.target)
        ctx << x.target << fieldAccessorFor(targetTy) << ctx.nameFor(found)
    }
  }

  override def visitLambdaType(
      x: types.EirLambdaType
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "auto"
  }

  override def visitProxyType(
      x: types.EirProxyType
  )(implicit ctx: CodeGenerationContext): Unit =
    ctx << ctx.nameFor(ctx.resolve(x))

  override def visitImport(
      x: EirImport
  )(implicit ctx: CodeGenerationContext): Unit = {
//    if (x.wildcard || x.qualified.length == 1) ctx << s"using namespace ${(if (x.wildcard) x.qualified.init else x.qualified) mkString "::"};"
//    else ctx << s"using ${x.qualified.last} = ${x.qualified mkString "::"};"
  }

  def isEntryArgument(f: EirFunctionArgument): Boolean = {
    f.parent
      .flatMap(_.parent)
      .flatMap(_.parent)
      .exists(_.isInstanceOf[EirProxy])
  }

  def splitIndex(
      ctx: CodeGenerationContext,
      t: EirType,
      curr: String
  ): Iterable[String] = {
    arrayDim(ctx, t) match {
      case Some(n) => (0 until n).map(idx => s"$curr[$idx]")
      case _       => Errors.unreachable()
    }
  }

  def castToPuppable(
      ctx: CodeGenerationContext,
      expr: EirExpressionNode,
      ours: EirType
  ): Unit = {
    if (ours.isTransient) {
      Errors.cannotSerialize(expr, ours)
    } else {
      ctx << expr
    }
  }

  def structToTrait(n: EirNode, c: EirClassLike, noCopy: Boolean)(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    if (noCopy) {
      ctx << "std::shared_ptr<" << ctx.typeFor(
        c,
        Some(n)
      ) << ">(std::shared_ptr<void>{},&" << n << ")"
    } else {
      ctx << "std::make_shared<" << ctx.typeFor(c, Some(n)) << ">(" << n << ")"
    }
  }

  def visitCallArgument(t: (EirExpressionNode, EirFunctionArgument))(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val requiresRef = t._1 match {
      case arg: EirCallArgument => arg.isRef
      case _                    => false
    }

    Find.resolutions[EirType](t._2.declaredType) match {
      case goal :: _ =>
        val shouldDeref = goal.isPointer && t._2.isReference
        ctx << Option.when(shouldDeref)("*(") << implicitCast(
          goal,
          t._1,
          requiresRef
        ) << Option.when(shouldDeref)(")")
      case Nil => ???
    }
  }

  def visitArguments(
      disambiguation: Option[EirNode],
      rawArgs: ArgumentList
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    visitArguments(disambiguation, rawArgs._1 ++ rawArgs._2)
  }

  // TODO direct use of this function should be considered suspect!
  def visitArguments(
      disambiguation: Option[EirNode],
      args: List[EirExpressionNode]
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    // TODO add support for expansions
    val theirs: List[EirFunctionArgument] =
      asMember(disambiguation).orElse(disambiguation) match {
        // TODO this should drop args for new~!
        case Some(_ @EirMember(_, f: EirFunction, _)) =>
          f.functionArgs ++ f.implicitArgs
        case Some(f: EirFunction) => f.functionArgs ++ f.implicitArgs
        case _                    => Nil
      }

    if (theirs.length == args.length) {
      val zipped = args.zip(theirs)

      if (zipped.nonEmpty) {
        zipped.init.foreach(pair => {
          visitCallArgument(pair)(ctx)
          ctx << ","
        })

        visitCallArgument(zipped.last)(ctx)
      } else {
        ctx
      }
    } else {
      ctx << (args, ",")
    }
  }

  def visitCallback(target: EirExpressionNode, isReduction: Boolean)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    target match {
      case EirScopedSymbol(_proxy, _) =>
        val proxy = _proxy.foundType.to[EirProxy]
        val found = asMember(Some(disambiguate(ctx, target)))
        if (proxy.isDefined && found.exists(_.isEntry)) {
          ctx << "CkCallback("
          ctx << GenerateProxies.indexFor(
            ctx,
            proxy.get,
            found.map(_.member).to[EirFunction].get
          )
          ctx << "," << _proxy << ")"
        } else {
          Errors.expectedCallback(target)
        }
      case _ => Errors.expectedCallback(target)
    }
  }

  def visitReducer(
      _target: EirExpressionNode
  )(implicit ctx: CodeGenerationContext): Unit = {
    val target = asMember(Some(disambiguate(ctx, _target)))
    val annotation =
      target.flatMap(_.annotation("system")).flatMap(_("reducer"))
    annotation match {
      case Some(l @ EirLiteral(_, EirLiteralTypes.String, _)) =>
        ctx << l.stripped
      case _ => Errors.expectedReducer(_target)
    }
  }

  // TODO all uses of this should be considered legacy/suspect!
  def flattenArgs(args: ArgumentList): List[EirExpressionNode] = {
    args._1.map(_.expr) ++ args._2
  }

  def handleOptionMember(
      ctx: CodeGenerationContext,
      m: EirMember,
      base: EirExpressionNode,
      args: List[EirExpressionNode]
  ): Unit = {
    val apl = m.name == "apply"
    val opt = base match {
      case s: EirScopedSymbol[_] => s.target
      case _ if apl              => base
      case _                     => Errors.unreachable()
    }
    val rsv =
      if (apl) assertValid[EirLambdaType](ctx.typeOf(opt)).to
      else ctx.typeOf(opt)
    val ty = ctx.resolve(rsv) match {
      case t: EirTemplatedType if t.args.length == 1 => ctx.resolve(t.args.head)
      case _                                         => Errors.unreachable()
    }
    val ptr = ty.isPointer
    val astr = Option.unless(ptr)("*")
    def wrap(ty: EirType): Option[String] =
      Option.unless(ty.isPointer)(
        s"std::make_shared<${ctx.typeFor(ty, Some(base))}>"
      )
    m.name match {
      case "get"                   => ctx << "(" << astr << opt << ")"
      case "apply" if args.isEmpty => ctx << "nullptr"
      case "apply" if args.nonEmpty =>
        if (ptr) ctx << args.head
        else ctx << wrap(ty) << "(" << args.head << ")"
      case "nonEmpty" | "isEmpty" =>
        ctx << "(" << opt << (if (m.name == "nonEmpty") "!="
                              else "==") << "nullptr" << ")"
      case "getOrElse" =>
        ctx << "(" << opt << "?" << astr << opt << ":" << args.head << ")"
      case "map" | "flatMap" =>
        val wrapped =
          Option
            .when(m.name == "map")(
              ctx.resolve(assertValid[EirLambdaType](ctx.typeOf(args.head)).to)
            )
            .flatMap(wrap)
        ctx << "(" << opt << "?" << wrapped << "((*" << args.head << ")(" << astr << opt << "))" << ":" << "nullptr" << ")"
      case _ => ???
    }
  }

  def handleFutureMember(
      ctx: CodeGenerationContext,
      m: EirMember,
      base: EirExpressionNode,
      args: List[EirExpressionNode]
  ): Unit = {
    val apl = m.name == "apply"
    val fut = base match {
      case s: EirScopedSymbol[_] => s.target
      case _ if apl              => base
      case _                     => Errors.unreachable()
    }
    val rsv =
      if (apl) assertValid[EirLambdaType](ctx.typeOf(fut)).to
      else ctx.typeOf(fut)
    val ty = ctx.resolve(rsv) match {
      case t: EirTemplatedType if t.args.length == 1 => ctx.resolve(t.args.head)
      case _                                         => Errors.unreachable()
    }
    val ptr = ty.isPointer
    m.name match {
      case "apply" =>
        ctx.proxy match {
          case Some(_) => ctx << "this->make_future()"
          case None =>
            ctx << "ergoline::make_future(" << {
              globals.implicitProxyName
            } << ")"
        }
      case "set" =>
        ctx << fut << ".set(hypercomm::pack_to_port({}," << (args, ",") << "))"
      case "get" =>
        val futureName = "f"
        val retTy = ctx.typeFor(ty, Some(base))
        ctx << "(([&](const hypercomm::future&" << futureName << ")" << "->" << retTy << "{"
        ctx << "auto cb = std::make_shared<hypercomm::resuming_callback<" << retTy << ">>(CthSelf());"
        ctx << "this->request_future(" << futureName << ", cb);"
        ctx << "if (!cb->ready()) CthSuspend();"
        updateLocalityContext(ctx)
        ctx << "return cb->value();"
        ctx << "})(" << fut << "))"
      case _ => ???
    }
  }

  def isOption(t: EirType): Boolean =
    t match {
      case t: EirClass =>
        (t.name == "option") && (t.parent == globals.ergolineModule)
      case _ => false
    }

  def isOption(t: Option[EirNode]): Boolean = t.to[EirType].exists(isOption)

  def isFuture(t: EirType): Boolean =
    t match {
      case t: EirClass => (t.name == "future") && (t.parent == globals.ckModule)
      case _           => false
    }

  def isFuture(t: Option[EirNode]): Boolean = t.to[EirType].exists(isFuture)

  type ArgumentList =
    (List[EirCallArgument], List[EirSymbol[EirImplicitDeclaration]])

  @tailrec
  def stripSection(
      target: EirExpressionNode,
      getProxy: Boolean = false
  ): EirExpressionNode = {
    target match {
      case EirScopedSymbol(target, _) => stripSection(target, getProxy)
      case EirArrayReference(_, target, section :: Nil) =>
        stripSection(if (getProxy) target else section, getProxy)
      case _ => target
    }
  }

  def visitContribute(
      proxy: EirProxy,
      target: EirExpressionNode,
      args: ArgumentList
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    proxy.kind match {
      case Some(EirSectionProxy) =>
        val argv = flattenArgs(args)
        ctx << "this->local_contribution(" << "ergoline::conv2section(" << stripSection(
          target
        ) << ")" << {
          if (argv.size == 1)
            ", hypercomm::make_unit_value(), ergoline::make_null_combiner()"
          else ???
        } << ", hypercomm::intercall(" << visitCallback(
          argv.last,
          isReduction = true
        ) << ")" << ")"
      case Some(EirElementProxy) =>
        ctx << "ergoline::contribute(this," << {
          visitCallback(
            flattenArgs(args) match {
              case List(value, reducer, target) =>
                ctx << value << "," << visitReducer(reducer) << ","
                target
              case List(target) => target
              case _            => Errors.unreachable()
            },
            isReduction = true
          )
        } << ")"
      case _ => Errors.unreachable()
    }
  }

  def epIndexFor(proxy: EirProxy, member: EirMember, hasArgs: Boolean)(implicit
      ctx: CodeGenerationContext
  ): String = {
    "CkIndex_" + proxy.baseName + "::" + ctx.nameFor(member) + "(" + {
      Option.when(hasArgs)("nullptr").getOrElse("")
    } + ")"
  }

  def visitMulticast(
      proxy: EirProxy,
      member: EirMember,
      target: EirExpressionNode,
      args: ArgumentList
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    val hasArgs = (args._1.size + args._2.size) != 0

    {
      stripSection(target, getProxy = true) match {
        case s: EirSymbol[_] if CheckTypes.isSelf(s) =>
          ctx << "this->broadcast("
        case s => ctx << "hypercomm::broadcast_to(" << s << ","
      }
    } << "ergoline::conv2section(" << stripSection(
      target
    ) << ").clone()" << "," << {
      epIndexFor(proxy, member, hasArgs)
    } << ", hypercomm::pack_to_port({}" << Option.when(hasArgs)(",") << {
      visitArguments(Some(member), args)
    } << ")" << ")"
  }

  def visitSystemCall(implicit
      ctx: CodeGenerationContext,
      target: EirExpressionNode,
      disambiguated: EirNode,
      args: ArgumentList
  ): Unit = {
    val base = target match {
      case f: EirScopedSymbol[_] => f.target
      case _                     => target
    }
    val system = disambiguated.annotation("system").get
    val operator = system("operator").exists(_.toBoolean)
    val static = system("static").exists(_.toBoolean)
    val invert = system("invert").exists(_.toBoolean)
    val invOp = if (invert) "!" else ""
    val cast = system("cast").exists(_.toBoolean)
    val name =
      system("alias").map(_.stripped).getOrElse(ctx.nameFor(disambiguated))
    disambiguated match {
      case _ if operator =>
        val flattened = flattenArgs(args)
        assert(flattened.size == 1)
        ctx << "(" << base << name << flattened.head << ")"
      case m @ EirMember(Some(p: EirProxy), _, _) if m.name == "contribute" =>
        visitContribute(p, target, args)
      case m @ EirMember(Some(p: EirProxy), _, _) if p.isSection =>
        visitMulticast(p, m, target, args)
      case EirMember(Some(p: EirProxy), _, _) =>
        name match {
          case "index" => ctx << selfIndex
          case "parent" =>
            ctx << s"(CProxy_${p.baseName}(" << base << {
              p.collective match {
                case Some("group" | "nodegroup")      => ".ckGetGroupID()))"
                case Some(s) if s.startsWith("array") => ".ckGetArrayID()))"
              }
            }
          case _ => error(target)
        }
      case m: EirMember if isOption(disambiguated.parent) =>
        handleOptionMember(ctx, m, target, flattenArgs(args))
      case m: EirMember if isFuture(disambiguated.parent) =>
        handleFutureMember(ctx, m, target, flattenArgs(args))
      case _: EirMember if static =>
        ctx << s"$name(" << {
          val baseArg = EirCallArgument(base, isRef = false)(None)
          visitArguments(Some(disambiguated), (baseArg +: args._1, args._2))
        } << ")"
      case EirMember(_, f: EirFunction, _) if cast =>
        ctx << s"((" << ctx.typeFor(f.returnType) << ")" << base << ")"
      case m: EirMember =>
        if (name == "apply")
          ctx << base << s"(" << visitArguments(
            Some(disambiguated),
            args
          ) << ")"
        else {
          val fqnOrDot =
            if (m.isStatic) "::" else fieldAccessorFor(ctx.exprType(base))
          ctx << invOp << base << s"$fqnOrDot$name(" << visitArguments(
            Some(disambiguated),
            args
          ) << ")"
        }
      case _: EirFunction if name == "CkPrintf" || name == "CkAbort" =>
        ctx << name << "(\"%s\\n\"," << "(" << {
          visitArguments(Some(disambiguated), args)
        } << ")" << ".c_str())"
      case _ =>
        ctx << s"($name(" << visitArguments(
          Some(disambiguated),
          args
        ) << "))"
    }
  }

  def disambiguate(
      ctx: CodeGenerationContext,
      x: EirExpressionNode
  ): EirNode = {
    x.disambiguation.getOrElse(x match {
      case x: EirResolvable[_] => ctx.resolve[EirNode](x)
      case x                   => x
    })
  }

  def visitSpecialization(
      s: EirSpecialization
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (s.types.nonEmpty) {
      val types = s.types.map(ctx.resolve[EirResolvable[EirType]])
      ctx << "<" << {
        for (t <- types.init) {
          ctx << ctx.typeFor(t)
          ctx << ","
        }
      } << ctx.typeFor(types.last) << ">"
    }
  }

  override def visitFunctionCall(
      x: EirFunctionCall
  )(implicit ctx: CodeGenerationContext): Unit = {
    val disambiguated = disambiguate(ctx, x.target)
    val member = asMember(Some(disambiguated))
    val implicits = CheckTypes.getImplicitArgs(disambiguated) map { i =>
      // TODO enforce implicitness
      val symbol = EirSymbol[EirImplicitDeclaration](Some(x), List(i.name))
      symbol.foundType = Some(ctx.typeOf(i.declaredType))
      symbol
    }
    val hasArgs = (x.args.size + implicits.size) != 0
    val isAsync = disambiguated.annotation("async").isDefined
    val shouldPack = member.exists {
      case m @ EirMember(Some(_: EirProxy), _, _) =>
        (hasArgs || isAsync) && (m.isEntry || m.isMailbox)
      // TODO this should be a local call (that does not involve packing!)
      case m: EirMember => hasArgs && m.isEntryOnly
    }
    val arrayAccessor = member.collect {
      case m: EirMember if isArray(ctx, m.base) => m.name
    }
    // bypass arguments for size (implicit field accessor)
    if (arrayAccessor.contains("size")) {
      ctx << x.target
      return
    }
    if (disambiguated.isSystem) {
      ctx << visitSystemCall(
        ctx,
        x.target,
        disambiguated,
        (x.args, implicits)
      )
    } else {
      if (isAsync) {
        val retTy = disambiguated match {
          case EirMember(_, f: EirFunction, _) => ctx.resolve(f.returnType)
          case f: EirFunction                  => ctx.resolve(f.returnType)
          case _                               => Errors.missingType(disambiguated)
        }
        ctx << "(([&](){" << ctx.typeFor(
          retTy
        ) << ctx.temporary << "=" << "this->make_future()" << ";"
      }
      val isPointer = x.target match {
        // TODO make this more robust
        case s: EirSymbol[_] =>
          ctx.resolve[EirNode](s) match {
            case _: EirDeclaration                  => true
            case EirMember(_, _: EirDeclaration, _) => true
            case _: EirFunctionArgument             => true
            case _                                  => false
          }
        case _ => false
      }
      if (isPointer) ctx << "(*"
      ctx << x.target
      if (isPointer) ctx << ")"
      ctx << visitSpecialization(x) << "(" << {
        if (shouldPack) {
          if (ctx.shouldRepack(x)) ctx << "ergoline::repack("
          else ctx << "hypercomm::pack("
        }
        if (isAsync) {
          ctx << ctx.temporary
          if (hasArgs) ctx << ","
        }
        ctx << visitArguments(Some(disambiguated), (x.args, implicits))
        ctx << Option.when(shouldPack)(")")
      } << ")"
      if (isAsync) {
        ctx << "; return" << ctx.temporary << ";" << "})())"
      }
    }
  }

  def fieldAccessorFor(x: EirType): String = {
    if (x.isPointer) "->"
    else "."
  }

  override def visitForLoop(
      x: EirForLoop
  )(implicit ctx: CodeGenerationContext): Unit = {
    val overlap = Option
      .when(!canReuseSentinel(x.parent))(x)
      .flatMap(_.annotation("overlap"))
    val senti = overlap.map(_ => {
      makeSentinel(ctx, all = true, grouped = false)
    })

    x.header match {
      case EirCStyleHeader(declaration, test, increment) =>
        ctx << s"for (" <| (declaration, ";") << test << ";" << {
          increment.foreach(_ => ctx.ignoreNext(";"))
          increment
        } << ")" << x.body
      case h: EirForAllHeader =>
        val fieldAccessor = fieldAccessorFor(ctx.exprType(h.expression))
        // TODO find a better name than it_
        ctx << "{" << "auto it_ =" << h.expression << ";" << "while (it_" << fieldAccessor << "hasNext()) {"
        if (h.identifiers.length == 1) {
          val ident = h.identifiers.head
          if (ident != "_") {
            ctx << "auto" << ident << "= "
          }
          ctx << "it_" << fieldAccessor << "next();"
        } else {
          // TODO add support for tuple pattern matching
          //      to for each loop ~!
          ???
        }
        ctx.ignoreNext("{")
        // TODO add declarations
        ctx << x.body << "}"
      case _ => Errors.unreachable()
    }

    senti.foreach(popSentinel)
  }

  override def visitWhileLoop(
      x: EirWhileLoop
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << s"while (" << x.condition << ")" << x.body
  }

  override def visitLiteral(
      x: EirLiteral
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.`type` match {
      case EirLiteralTypes.Unit   =>
      case EirLiteralTypes.String => ctx << s"std::string(${x.value})"
      case _                      => ctx << x.value
    }
  }

  def selfFor(ctx: CodeGenerationContext, x: EirMember): String = {
    ctx.proxy
      .collect({
        case _ if x.isImplOnly => "impl_"
      })
      .getOrElse("this")
  }

  def asMember(x: Option[EirNode]): Option[EirMember] = {
    x match {
      case Some(m: EirMember) => Some(m)
      case _                  => x.flatMap(_.parent).to[EirMember]
    }
  }

  override def visitSymbol[A <: EirNamedNode](
      x: EirSymbol[A]
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (!CheckTypes.isSelf(x)) {
      val m = asMember(x.disambiguation)
      if (!m.exists(_.isStatic)) m.foreach(ctx << selfFor(ctx, _) << "->")
    }
    val m = asMember(x.disambiguation) match {
      case Some(m) if m.isEntryOnly =>
        Some(Find.namedChild[EirMember](ctx.proxy, m.name))
      case _ => None
    }
    ctx << m.map(ctx.nameFor(_)).getOrElse(ctx.nameFor(x, Some(x)))
  }

  override def visitDeclaration(
      x: EirDeclaration
  )(implicit ctx: CodeGenerationContext): Unit = {
    val lhsTy = ctx.resolve(x.declaredType)
    ctx << ctx.typeFor(lhsTy, Some(x)) << s"${ctx.nameFor(x)}" << {
      x.initialValue.foreach { x => assignmentRhs(lhsTy, "=", x) }
    } << ";"
  }

  override def visitTemplateArgument(
      x: EirTemplateArgument
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << s"typename ${x match {
      case _ if x.isPack => s"... ${x.name}"
      case _             => x.name
    }}"
  }

  def visitInherits(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    val base = x.extendsThis.map(ctx.resolve)
    val parents =
      (base ++ x.implementsThese.map(ctx.resolve)) map (ctx.nameFor(_))
    val pupableBase = base.map(ctx.typeOf).exists(!_.isTrait)

    if (x.isInstanceOf[EirTrait]) {
      ctx << {
        if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(",")
        else ": public hypercomm::polymorph::trait"
      }
    } else {
      ctx << ": " << {
//        if (parents.isEmpty) "public ergoline::object" else
        parents.map("public " + _).mkString(", ")
      }
      if (!pupableBase) {
        // TODO x.isTransient ?
        ctx << Option.when(parents.nonEmpty)(",") << "public ergoline::object"
      }
      ctx <<
        Option.unless(x.isValueType)(
          ", public std::enable_shared_from_this<" + nameFor(
            ctx,
            x,
            includeTemplates = true
          ) + ">"
        )
    }
  }

  def puppingParents(
      ctx: CodeGenerationContext,
      x: EirClassLike
  ): List[EirType] = {
    // TODO enable stateful traits?
    // TODO be templating aware?
    (x.extendsThis ++ x.implementsThese)
      // NOTE -- this is a sketchy use of EirType!
      .map(ctx.resolve[EirType])
      .filterNot(_.isTrait)
      .toList
  }

  def pupperFor(
      ctx: (CodeGenerationContext, EirNode, String)
  )(current: String, ours: EirType): List[String] = {
    List(s"${ctx._3} | $current;")
  }

  def makePupper(
      ctx: CodeGenerationContext,
      x: EirClassLike,
      isMember: Boolean = false
  ): Unit = {
    // TODO check to ensure user does not override
    val puper = ctx.temporary
    val header =
      if (isMember) s"virtual void __pup__(hypercomm::serdes &$puper) override"
      else s"void ${ctx.nameFor(x)}::__pup__(hypercomm::serdes &$puper)"
    ctx << header << "{"

    if (x.isTransient) {
      ctx << "CkAbort(\"cannot pup transient types\");"
    } else {
      val parents = puppingParents(ctx, x)
        .map(ctx.nameFor(_))
        .map(_ + s"::__pup__($puper);")
      val values = x.members
        .collect({
          case m @ EirMember(_, d: EirDeclaration, _)
              if m.annotation("transient").isEmpty && !m.isStatic =>
            d
        })
        .flatMap(d => {
          pupperFor((ctx, x, puper))(
            ctx.nameFor(d),
            ctx.resolve(d.declaredType)
          )
        })
      ctx << (parents ++ values)
    }

    ctx << "}"
  }

  def hasherFor(
      ctx: CodeGenerationContext,
      d: EirDeclaration,
      hasher: String
  ): Unit = {
    ctx << s"$hasher | ${ctx.nameFor(d)};"
  }

  def makeHasher(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    val hasher = ctx.temporary
    ctx << s"virtual hypercomm::hash_code hash(void) const override" << "{"
//    ctx << "ergoline::hasher" << hasher << ";"
//    // TODO combine with hash of parents
//    val members = x.members.collect({
//      // TODO should we consider transient or nah?
//      case m@EirMember(_, d: EirDeclaration, _) if m.annotation("hashExclude").isEmpty && !m.isStatic => d
//    })
//    if (members.nonEmpty) {
//      members.foreach(hasherFor(ctx, _, hasher))
//    } else {
//      ctx << s"$hasher | typeid(this).hash_code();"
//    }
//    ctx << s"return $hasher.hash();"
    ctx << "return typeid(this).hash_code();"
    ctx << "}"
  }

  def visitClassLike(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (x.templateArgs.isEmpty) {
      val isSystem = x.annotation("system").isDefined
      ctx << x.members.filter(_.member.isInstanceOf[EirFunction])
      if (!x.isTrait && !isSystem && !GenerateDecls.hasPup(x)) {
        makePupper(ctx, x)
      }
    }
  }

  override def visitMember(
      x: EirMember
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << Option.when(x.isStatic)("static")
    visit(x.member)
  }

  def visitTemplateArgs(
      args: List[EirTemplateArgument]
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (args.nonEmpty) ctx << s"template<" << (args, ",") << "> "
  }

  def generateAssignments(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx << x.functionArgs
      .filter(_.isSelfAssigning)
      .map(x => {
        "this->" + x.name + "=" + x.name + ";"
      })
  }

  def visitFunctionBody(
      x: EirFunction
  )(implicit ctx: CodeGenerationContext): Unit = {
    val member = x.parent.to[EirMember]

    if (member.exists(_.isConstructor)) {
      val parent = assertValid[EirClassLike](member.flatMap(_.parent))
      val parentClass =
        parent.extendsThis.map(ctx.resolve).flatMap(Find.tryClassLike)

      val encapsulated = ctx.proxy.nonEmpty
      val currSelf = if (encapsulated) "impl_" else "this"
      val assignments = x.functionArgs.filter(_.isSelfAssigning)
      val declarations = parent.members collect {
        case m @ EirMember(_, d: EirDeclaration, _)
            if !m.isStatic && d.initialValue.nonEmpty =>
          d
      }

      if (!encapsulated) {
        ctx << Option.when(assignments.nonEmpty || declarations.nonEmpty)(":")

        // TODO find whether the body contains a call to (super) and isolate it.

        ctx << (assignments.map(x => {
          val name = ctx.nameFor(x)
          s"$name($name)"
        }), ",")
        ctx << Option.when(assignments.nonEmpty && declarations.nonEmpty)(",")
        ctx << (declarations.map(x => {
          (ctx.makeSubContext() << ctx.nameFor(
            x
          ) << "(" << x.initialValue << ")").toString
        }), ",")
      }

      if (assignments.nonEmpty || declarations.nonEmpty) {
        ctx << "{"
        ctx.ignoreNext("{")
      }

      if (encapsulated) {
        ctx << assignments.map(arg => {
          val name = ctx.nameFor(arg)
          currSelf + "->" + name + "=" + name + ";"
        })

        declarations.foreach(d => {
          ctx << currSelf << "->" << ctx.nameFor(
            d
          ) << "=" << d.initialValue << ";"
        })
      }
    }

    ctx <| (x.body, ";")
  }

  def visitFunction(x: EirFunction, isMember: Boolean)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    val member = x.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val entryOnly = member.exists(_.isEntryOnly) && ctx.proxy.isEmpty
    val system = member
      .flatMap(_.annotation("system"))
      .orElse(x.annotation("system"))
      .isDefined
    val abstractMember =
      !isMember && (parent.exists(_.isAbstract) && x.body.isEmpty)
    val langCi = ctx.language == "ci"
    val isTempl = parent.isDefined && !isMember && x.templateArgs.nonEmpty
    if ((!langCi && entryOnly) || system || abstractMember || isTempl) {
      return
    }
    val asyncCi =
      langCi && isMember && member.flatMap(_.annotation("async")).isDefined
    val isConstructor = member.exists(_.isConstructor)
    val overrides =
      Option.when(isMember && member.exists(_.isOverride))(" override")
    val name = parent match {
      case Some(p: EirProxy) if langCi && isConstructor => p.baseName
      case Some(classLike) if !isMember =>
        ctx.nameFor(classLike) + "::" + ctx.nameFor(x)
      case _ => ctx.nameFor(x)
    }
    val virtual =
      Option.when(isMember && !langCi && member.exists(_.isVirtual))("virtual")
    visitTemplateArgs(x.templateArgs)
    ctx << virtual
    // TODO add templates when !isMember
    if (asyncCi) {
      ctx << "void"
    } else {
      ctx << Option.when(!isConstructor)(ctx.typeFor(x.returnType))
    }
    val args = x.functionArgs ++ x.implicitArgs
    ctx << name << "("
    if (parent.exists(_.isInstanceOf[EirProxy]) && (args.nonEmpty || asyncCi)) {
      ctx << "CkMessage* __msg__"
    } else {
      ctx << (args, ",")
    }
    ctx << ")" << overrides
    if (isMember) {
      if (virtual.nonEmpty && x.body.isEmpty) {
        ctx << " = 0;"
        return
      } else if (
        langCi || (!parent.exists(
          _.templateArgs.nonEmpty
        ) && x.templateArgs.isEmpty)
      ) {
        ctx << ";"
        return
      }
    } else if (x.body.isEmpty) {
      Errors.missingBody(x)
    }
    visitFunctionBody(x)
  }

  override def visitFunction(
      x: EirFunction
  )(implicit ctx: CodeGenerationContext): Unit =
    visitFunction(x, isMember = false)

  def visitAnnotations(
      annotations: Iterable[EirAnnotation]
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "/* " << (annotations.map(_.toString), " ") << " */ "
  }

  override def visitUnaryExpression(
      x: EirUnaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.disambiguation match {
      case Some(x) => ctx << x
      case None =>
        ctx << "(" << x.op << "(" << x.rhs << "))"
    }
  }

  override def visitBinaryExpression(
      x: EirBinaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.disambiguation match {
      case Some(x) => ctx << x
      case None =>
        ctx << "(" << x.lhs << {
          Option
            .when(globals.isIdentityComparator(x.op))(x.op.init)
            .getOrElse(x.op)
        } << x.rhs << ")"
    }
  }

  def qualificationsFor(node: EirNode, within: EirNode)(implicit
      ctx: CodeGenerationContext
  ): Seq[String] = {
    val base = node match {
      case t: EirTemplatedType => ctx.resolve(t.base)
      case _                   => node
    }

    if (base.annotation("system").isDefined) {
      return Nil
    }

    val ours = Find.parentOf[EirNamespace](within)
    val theirs = Find.parentOf[EirNamespace](base)

    theirs match {
      case Some(theirs) if !ours.contains(theirs) =>
        (theirs.name +: Find.ancestors(theirs).collect {
          case n: EirNamespace => n.name
        }).reverse
      case _ => Nil
    }
  }

  def qualifiedNameFor(
      ctx: CodeGenerationContext,
      occurrence: EirNode,
      includeTemplates: Boolean = false
  )(
      of: EirNode
  ): String = {
    (qualificationsFor(of, occurrence)(ctx) :+ nameFor(
      ctx,
      of,
      includeTemplates,
      Some(occurrence)
    )).mkString("::")
  }

  def templateArgumentsToString(
      ctx: CodeGenerationContext,
      args: List[EirResolvable[EirType]],
      usage: Option[EirNode]
  ): String = {
    "<" + {
      args
        .map(ctx.resolve[EirResolvable[EirType]])
        .map(ctx.typeFor(_, usage))
        .mkString(",")
    } + ">"
  }

  def selfIndex: String = "this->__index__()"

  def selfName(ctx: CodeGenerationContext, s: EirSymbol[_]): String = {
    s.qualifiedName.last match {
      case "self@" => {
        ctx.proxy
          .flatMap(_.collective)
          .map(_ => "this->thisProxy")
          .getOrElse("this->thisProxy[this->thisIndexMax]")
      }
      case "self[@]" =>
        s"this->thisProxy[" + {
          s"hypercomm::conv2idx<CkArrayIndex>($selfIndex)"
        } + "]"
      case _ => selfName(ctx, s.asInstanceOf[EirNode])
    }
  }

  def selfName(ctx: CodeGenerationContext, n: EirNode): String = {
    val ty = n match {
      case e: EirExpressionNode               => ctx.exprType(e)
      case d: EirDeclaration                  => ctx.resolve(d.declaredType)
      case EirMember(_, d: EirDeclaration, _) => ctx.resolve(d.declaredType)
      case _                                  => Errors.missingType(n)
    }

    if (ty.isPointer) {
      "(" + nameFor(ctx, ty, includeTemplates = true) + "::shared_from_this())"
    } else {
      "*this"
    }
  }

  def nameFor(
      ctx: CodeGenerationContext,
      x: EirNode,
      includeTemplates: Boolean = false,
      usage: Option[EirNode] = None
  ): String = {
    val system = x.annotation("system")
    val alias =
      system.flatMap(_("alias")).map(_.stripped)
    val isOperator = system.flatMap(_("operator")).exists(_.toBoolean)
    val dealiased = alias
      .orElse(x match {
        case n: EirNamedNode =>
          if (isOperator || n.name.forall(x => x.isLetterOrDigit || x == '_')) Some(n.name)
          else Some(globals.encodeOperator(n.name))
        case _               => None
      })
      .map(x => {
        if (x == "std::size_t" && ctx.language == "ci") "size_t"
        else x
      })
    val opt = Some(x)
    val proxy = opt
      .to[EirType]
      .flatMap(ProxyManager.asProxy)
      .orElse(asMember(opt).flatMap(_.parent.to[EirProxy]))
    val result = x match {
      case s: EirSymbol[_] =>
        if (CheckTypes.isSelf(s)) {
          selfName(ctx, s)
        } else {
          val rsv = ctx.tryResolve[EirNode](s)
          rsv match {
            case Some(n: EirNode) =>
              // TODO need to use FQN here, symbol is self-context providing
              nameFor(ctx, n, includeTemplates, usage.orElse(Some(s)))
            case None
                if s.qualifiedName.lastOption.contains(
                  globals.implicitProxyName
                ) =>
              s"(this->__element__())"
            case _ => Errors.unableToResolve(s)
          }
        }
      case _: EirMember | _: EirFunction if proxy.isDefined =>
        (x, proxy.flatMap(_.ordinalFor(x))) match {
          case (x: EirNamedNode, Some(ord)) => s"__${x.name}_${ord}__"
          case (x: EirNamedNode, _)         => x.name
          case (_, _)                       => Errors.unreachable()
        }
      case _ if proxy.isDefined =>
        val prefix =
          if (proxy.exists(_.singleton)) "CProxyElement_" else "CProxy_"
        val name = prefix + proxy.get.baseName
        x match {
          case t: EirTemplatedType =>
            name + templateArgumentsToString(ctx, t.args, usage)
          case _ => name
        }
      case _ if dealiased.contains("self") => selfName(ctx, x)
      case x: EirLambdaType =>
        val args = (x.to +: x.from).map(ctx.typeFor(_, Some(x)))
        s"ergoline::function<${args.mkString(",")}>"
      case x: EirTemplateArgument =>
        (ctx.hasSubstitution(x) match {
          case Some(t) => ctx.nameFor(t)
          case None    => dealiased.get
        }) + (if (x.isPack) "..." else "")
      case x: EirTemplatedType =>
        arrayDim(ctx, x) match {
          case Some(n) =>
            s"ergoline::array<${ctx.typeFor(arrayElementType(x), usage)}, $n>"
          case None => {
            val base = ctx.resolve(x.base)
            nameFor(ctx, base, usage = usage) +
              Option
                .unless(isFuture(base))({
                  templateArgumentsToString(ctx, x.args, usage)
                })
                .getOrElse("")
          }
        }
      case x: EirSpecializable with EirNamedNode if x.templateArgs.nonEmpty =>
        val subst = x.templateArgs.map(ctx.hasSubstitution)
        val substDefined = subst.forall(_.isDefined)
        dealiased.get + (if (includeTemplates || substDefined) {
                           "<" + {
                             if (substDefined)
                               subst.flatten.map(ctx.typeFor(_, usage))
                             else x.templateArgs.map(ctx.typeFor(_, usage))
                           }.mkString(",") + ">"
                         } else "")
      case t: EirTupleType =>
        s"std::tuple${templateArgumentsToString(ctx, t.children, usage)}"
      case _: EirNamedNode      => dealiased.get
      case s: EirConstantFacade => s.value.value
      case x: EirLambdaExpression =>
        _lambda_names.get(x) match {
          case Some(name) => name
          case None =>
            val name = x.location
              .map((i: EirSourceInfo) =>
                s"__lambda__${((s: String) => {
                  s.substring(0, s.indexOf('.'))
                })(Paths.get(i.sourceName).getFileName.toString)}__L${i.line}C${i.start}__"
              )
              .getOrElse(Errors.unableToName(x))
            _lambda_names.put(x, name)
            name
        }
    }
    if (ctx.hasPointerOverride(x)) s"(*$result)" else result
  }

  def typeForEntryArgument(
      ctx: (CodeGenerationContext, EirNode)
  )(ty: EirResolvable[EirType]): String = {
    val resolution = ctx._1.resolve[EirNode](ty)
    resolution match {
      case _: EirTemplateArgument => ctx._1.nameFor(resolution, Some(ctx._2))
      case t: EirTupleType =>
        "std::tuple<" + {
          t.children
            .map(ctx._1.resolve[EirType])
            .map(typeForEntryArgument(ctx))
            .mkString(",")
        } + ">"
      case t: EirLambdaType            => ctx._1.typeFor(t, Some(ctx._2))
      case t: EirType if t.isTransient => Errors.cannotSerialize(ctx._2, t)
      // TODO exempt system types here?
      case t: EirType if t.isTrait => "std::shared_ptr<PUP::able>"
      case t: EirType              => ctx._1.typeFor(t, Some(ctx._2))
      case _                       => Errors.incorrectType(resolution, classOf[EirType])
    }
  }

  override def visitFunctionArgument(
      x: EirFunctionArgument
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << {
      if (isEntryArgument(x))
        ctx << typeForEntryArgument((ctx, x))(x.declaredType)
      else {
        val ty = ctx.resolve(x.declaredType)
        ctx << ctx.typeFor(ty, Some(x)) << Option.when(
          x.isReference && Find.tryClassLike(ty).exists(_.isValueType)
        )("&")
      }
    } << ctx.nameFor(x)
  }

  override def visitTupleExpression(
      x: EirTupleExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    val func = x.parent match {
      case Some(a: EirAssignment) if a.lval == x => "tie"
      case _                                     => "make_tuple"
    }
    ctx << s"std::$func(" << (x.children, ",") << ")"
  }

  private val _lambda_names: mutable.Map[EirLambdaExpression, String] =
    mutable.Map()

  override def visitLambdaExpression(
      x: EirLambdaExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    val captures = x.captures.map(captured => {
      // TODO use specialized version when avail
      val ty = ctx.typeOf(captured)
      val name = ctx.nameFor(captured, Some(x))
      if (ty.isPointer) name
      else {
        val t = ctx.typeFor(ty, Some(x))
        s"std::shared_ptr<$t>(std::shared_ptr<$t>{}, &$name)"
      }
    })
    ctx << s"std::make_shared<${ctx.nameFor(x)}>(${captures mkString ","})"
  }

  def makeLambdaWrapper(
      ctx: CodeGenerationContext,
      lambda: EirLambdaExpression
  ): Unit = {
    val name = ctx.nameFor(lambda)
    val captures = lambda.captures
    val ty = assertValid[EirLambdaType](ctx.exprType(lambda))
    assert(ty.templateArgs.isEmpty)
    val args = (ty.to +: ty.from).map(ctx.typeFor(_))
    val ctypes = captures.map(ctx.typeOf(_))
    val isTransient = ctypes.exists(_.isTransient)
    val cdecltypes = ctypes.map(_t => {
      val t = ctx.typeFor(_t, Some(lambda))
      if (_t.isPointer) t else s"std::shared_ptr<$t>"
    })
    ctx << s"struct $name: public ergoline::function<${args mkString ","}> {"
    ctx << s"$name(${cdecltypes.zipWithIndex.map({
      case (t, idx) => s"$t _$idx"
    }) mkString ","})" << Option.when(captures.nonEmpty)(": ") << {
      (
        captures.zipWithIndex.map({
          case (n, idx) => s"${n.name}(_$idx)"
        }),
        ","
      )
    } << "{ }"
    //    if () ctx << s"$name() { }"
    ctx << s"$name(PUP::reconstruct __tag__): ergoline::function<${args mkString ","}>(__tag__) { }"
    ctx << "virtual void __pup__(hypercomm::serdes& _) override" << "{"
    if (isTransient) {
      ctx << "CkAbort(\"lambda" << name << "is transient and cannot be pup'd.\""
    } else {
      ctx << captures
        .zip(ctypes)
        .flatMap({
          case (n, t) => pupperFor((ctx, lambda, "_"))(n.name, t)
        })
    }
    ctx << "}"
    ctx << s"virtual" << args.head << "operator()("
    ctx << (lambda.args
      .zip(args.tail)
      .map({ case (arg, ty) => s"$ty ${arg.name}" }), ",")
    ctx << ") override"
    val crefs = captures.zip(ctypes).collect {
      case (x, t) if !t.isPointer => x
    }
    crefs.foreach(ctx.makePointer)
    ctx << lambda.body
    crefs.foreach(ctx.unsetPointer)
    ctx << captures
      .zip(cdecltypes)
      .map({
        case (n, t) => s"$t ${n.name};"
      })
    ctx << "};"
  }

  @tailrec
  def isArray(ctx: CodeGenerationContext, t: EirType): Boolean = {
    t match {
      case t: EirTemplatedType => isArray(ctx, ctx.resolve(t.base))
      case c: EirClass =>
        c.name == "array" && c.parent == globals.ergolineModule
      case _ => false
    }
  }

  def containsArray(
      ctx: CodeGenerationContext,
      t: EirResolvable[EirType]
  ): Boolean = {
    ctx.resolve(t) match {
      case t: EirTupleType => t.children.exists(containsArray(ctx, _))
      case t               => isArray(ctx, t)
    }
  }

  def arrayDim(ctx: CodeGenerationContext, t: EirType): Option[Int] = {
    ctx.resolve(t) match {
      case t: EirTemplatedType if isArray(ctx, t) =>
        t.args match {
          case _ +: Nil      => Some(1)
          case _ +: t +: Nil => Some(ctx.eval2const(t).toInt)
          case _             => None
        }
      case _ => None
    }
  }

  def arrayElementType(t: EirType): EirResolvable[EirType] = {
    t match {
      case t: EirTemplatedType => t.args.head
      case _                   => Errors.unreachable()
    }
  }

  def makeIndex(
      ctx: CodeGenerationContext,
      args: List[EirExpressionNode]
  ): Unit = {
    ctx << "{ (std::size_t) " << (args, ", (std::size_t) ") << "})"
  }

  override def visitNew(
      x: EirNew
  )(implicit ctx: CodeGenerationContext): Unit = {
    val objTy: EirType = ctx.resolve(x.target)
    val proxy = ProxyManager.asProxy(objTy)
    val collective = proxy.flatMap(_.collective)
    val args = x.args
    (objTy, proxy) match {
      case (_, Some(p)) =>
        val numTake =
          collective match {
            // TODO add support for unbound arrays?
            case Some(ProxyManager.arrayPtn(dim)) =>
              ctx << "(" << ctx.nameFor(objTy, Some(x)) << "("
              ctx << ctx.nameFor(objTy, Some(x)) << s"::ckNew("
              dim.toInt
            case Some(s @ ("group" | "nodegroup")) =>
              ctx << ("((hypercomm::make_" + s + "like") << "<"
              ctx << ctx.nameFor(objTy, Some(x)) << ">("
              0
            case None =>
              // TODO use a custom command that returns a proxy
//              ctx << "((" << GenerateCi.readOnlyFor(p, Some(x)) << "[" << {
//                indexForSingleton(p)
//              } << "]" << ".insert("
              val types = objTy match {
                case t: EirTemplatedType => t.types
                case _                   => Nil
              }
              ctx << "((ergoline::create_element(" << {
                readOnlyFor(p, types, Some(x))
              } << "," << {
                indexForSingleton(p, types)
              } << Option.when(args.nonEmpty)(",")
              0
          }
        val postDrop = args.drop(numTake)
        if (postDrop.nonEmpty) {
          ctx << "hypercomm::pack(" << {
            visitArguments(x.disambiguation, postDrop)
          } << ")" << Option.unless(numTake == 0)(",")
        }
        if (numTake > 0) {
          ctx << "CkArrayOptions("
          if (numTake == 1) {
            ctx << args.head
          } else {
            ctx << (args.slice(0, numTake), ",")
          }
          ctx << ")"
        }
        ctx << ")" << ")" << ")"
      case (t: EirType, None) if t.isPointer =>
        ctx << "std::make_shared<" << ctx.nameFor(t, Some(x)) << ">("
        arrayDim(ctx, t) match {
          case Some(n) =>
            ctx << "ergoline::array<" << ctx.typeFor(
              arrayElementType(t),
              Some(x)
            ) << "," << n.toString << ">(" << makeIndex(ctx, args)
          case None => visitArguments(x.disambiguation, args)
        }
        ctx << ")"
      case _ =>
        ctx << Option.when(objTy.isPointer)("new") << ctx.typeFor(
          objTy,
          Some(x)
        ) << "(" << visitArguments(x.disambiguation, args) << ")"
    }
  }

  override def visitMatch(
      x: EirMatch
  )(implicit ctx: CodeGenerationContext): Unit = {
    // TODO restore failure to match CmiAbort/throw!
    ctx << s"([&](" << ctx.typeFor(
      ctx.exprType(x.expression)
    ) << s"${ctx.temporary}) ->" << ctx.typeFor(
      ctx.exprType(x)
    ) << "{" << x.cases << {
      val location = Errors.contextualize(x)
      "CkAbort(\"no match found at " + location.substring(
        location.lastIndexOf(File.separator) + 1
      ) + "\");"
    } << "})(" << x.expression << ")"
  }

  def visitPatternDecl(
      parent: CodeGenerationContext,
      x: EirPattern,
      current: String,
      forceTuple: Boolean = false
  ): String = {
    implicit val ctx = parent.makeSubContext()
    x match {
      case EirPatternList(_, ps) =>
        ps match {
          case p :: Nil if !forceTuple =>
            ctx << visitPatternDecl(ctx, p, current)
          case p :: Nil =>
            ctx << visitPatternDecl(ctx, p, s"std::get<0>($current)")
          case patterns =>
            ctx << patterns.zipWithIndex.map({
              case (p, idx) =>
                visitPatternDecl(ctx, p, s"std::get<$idx>($current)")
            })
        }
      case i @ EirIdentifierPattern(_, n, t) if n != "_" =>
        val ty = ctx.resolve(t)
        ctx.ignoreNext(";")
        if (ty.isPointer && i.needsCasting)
          ctx << i.declarations.head << s" = std::dynamic_pointer_cast<${ctx
            .nameFor(t)}>($current);"
        // TODO make this a reference!
        else ctx << i.declarations.head << s" = $current;"
      case i: EirIdentifierPattern =>
        if (i.name != "_") Errors.missingType(x)
      case _: EirExpressionPattern =>
    }
    ctx.toString
  }

  def typeAt(
      ctx: CodeGenerationContext,
      t: Option[EirType],
      idx: Int
  ): Option[EirType] = {
    t match {
      case Some(t: EirTupleType) if idx < t.children.length =>
        val res = ctx.resolve[EirResolvable[EirType]](t.children(idx))
        Some(ctx.typeOf(res))
      case _ => None
    }
  }

  def visitPatternCond(
      parent: CodeGenerationContext,
      x: EirPattern,
      current: String,
      parentType: Option[EirType]
  ): List[String] = {
    x match {
      case EirPatternList(_, ps) =>
        ps match {
          case p :: Nil =>
            parentType match {
              case Some(t: EirTupleType) =>
                visitPatternCond(
                  parent,
                  p,
                  s"std::get<0>($current)",
                  t.children.headOption.map(parent.resolve[EirType])
                )
              case _ => visitPatternCond(parent, p, current, parentType)
            }
          case patterns =>
            patterns.zipWithIndex.flatMap {
              case (p, idx) =>
                visitPatternCond(
                  parent,
                  p,
                  s"std::get<$idx>($current)",
                  typeAt(parent, parentType, idx)
                )
            }
        }
      case i @ EirIdentifierPattern(_, n, t) if parent.resolve(t).isPointer =>
        val wildcard = n == "_"
        parentType match {
          case None => Errors.missingType(x)
          // TODO use a more reliable comparison here!
          case Some(u) if !i.needsCasting => Nil
          case _                          =>
            // TODO this needs to inherit substitutions
            //      (when such things are added)
            val ctx = parent.makeSubContext()
            List("(bool)" + {
              if (wildcard)
                s"std::dynamic_pointer_cast<${ctx.nameFor(t)}>($current)"
              else n
            })
        }
      case _: EirIdentifierPattern => Nil
      case e: EirExpressionPattern =>
        val ctx = parent.makeSubContext()
        ctx.putReplacement("_", current)
        (ctx << e.expression).toString.split(n).toList
    }
  }

  override def visitMatchCase(
      x: EirMatchCase
  )(implicit ctx: CodeGenerationContext): Unit = {
    val parent = x.parent.to[EirMatch]
    val exprType = parent.map(_.expression).map(ctx.exprType)
    val isUnit = parent
      .map(ctx.exprType)
      .contains(globals.typeFor(EirLiteralTypes.Unit))
    ctx << "{" << visitPatternDecl(ctx, x.patterns, ctx.temporary).split(n)
    val conditions = visitPatternCond(ctx, x.patterns, ctx.temporary, exprType)
      .mkString(" && ")
    val needsIf = x.condition.nonEmpty || conditions.nonEmpty
    if (needsIf) ctx << "if(" << x.condition << {
      Option.when(x.condition.isDefined && conditions.nonEmpty)(" && ")
    } << conditions << ")" << "{"
    val (primary, secondary) =
      (Option.unless(isUnit)("return"), Option.when(isUnit)("return;"))
    ctx << primary << x.body << ";" << secondary << Option.when(needsIf)(
      "}"
    ) << "}"
  }

  override def visitTupleType(
      x: types.EirTupleType
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << s"std::tuple<"
    if (x.children.nonEmpty) {
      for (t <- x.children.init) {
        ctx << ctx.typeFor(t) << ","
      }
      ctx << ctx.typeFor(x.children.last)
    }
    ctx << ">"
  }

  override def visitArrayReference(
      arrayRef: EirArrayReference
  )(implicit ctx: CodeGenerationContext): Unit = {
    val ty = ctx.exprType(arrayRef.target)
    val collective = ProxyManager
      .asProxy(ty)
      .flatMap(x => if (!x.isElement) x.collective else None)
    ty match {
      case tty: EirTupleType =>
        val arg = arrayRef.args.headOption.map(
          CheckTypes.evaluateConstExpr(_)(ctx.typeContext)
        )
        arg match {
          case Some(x) =>
            ctx << s"std::get<" << x << ">(" << arrayRef.target << ")"
          case None => Errors.invalidTupleIndices(tty, arrayRef.args)
        }
      case _ if collective.isDefined =>
        ctx << arrayRef.target << "[" << "hypercomm::conv2idx<CkArrayIndex>(" << {
          if (arrayRef.args.length == 1) {
            ctx << arrayRef.args.head
          } else {
            ctx << "std::make_tuple(" << (arrayRef.args, ",") << ")"
          }
        } << ")" << "]"
      case t if isArray(ctx, t) =>
        val target = arrayRef.target
        val args = arrayRef.args
        ctx << "(*" << target << ")["
        args.reverse.init.zipWithIndex.foreach {
          case (arg, idx) =>
            ctx << arg << s"* (" << target << s"->shape[$idx]) +"
        }
        ctx << args.head
        ctx << "]"
      case t =>
        if (isPlainArrayRef(arrayRef)) {
          if (t.isPointer) {
            ctx << "(*" << arrayRef.target << ")"
          } else {
            ctx << arrayRef.target
          }
          for (arg <- arrayRef.args) {
            ctx << "[" << arg << "]"
          }
        } else {
          ctx << arrayRef.disambiguation
        }
    }
  }

  override def visitSpecializedSymbol(
      x: EirSpecializedSymbol
  )(implicit ctx: CodeGenerationContext): Unit = {
    val base = ctx.resolve(x.symbol)
    ctx << ctx.nameFor(base) << visitSpecialization(x)
  }

  override def visitIfElse(
      x: EirIfElse
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "if (" << x.test << ")" << x.ifTrue << x.ifFalse.map(_ =>
      "else "
    ) << x.ifFalse
  }

  override def visitTernaryOperator(
      x: EirTernaryOperator
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "(" << x.test << "?" << x.ifTrue << ":" << x.ifFalse << ")"
  }

  override def visitTemplatedType(
      x: EirTemplatedType
  )(implicit ctx: CodeGenerationContext): Unit = {
    val base = ctx.resolve(x.base)
    ctx << ctx.nameFor(base) << "<"
    if (x.args.nonEmpty) {
      x.args.init.foreach(t => {
        ctx << ctx.typeFor(t, Some(x))
        ctx << ","
      })
      ctx << ctx.typeFor(x.args.last, Some(x))
    }
    ctx << ">"
  }

  override def visitBlock(
      x: EirBlock
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "{"
    x.children.foreach {
      case x: EirExpressionNode => ctx << x << ";"
      case x                    => ctx << x
    }
    ctx << "}"
  }

  override def visitNamespace(
      x: EirNamespace
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "namespace" << x.name << "{" << x.children << "}"
  }

  override def visitClass(x: EirClass)(implicit
      ctx: CodeGenerationContext
  ): Unit = visitClassLike(x)

  override def visitTrait(x: EirTrait)(implicit
      ctx: CodeGenerationContext
  ): Unit = visitClassLike(x)

  override def visitAnnotation(
      x: EirAnnotation
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << x.toString
  }

  def arrayRefIsSystem(x: EirArrayReference): Option[EirAnnotation] = {
    x.disambiguation
      .to[EirFunctionCall]
      .flatMap(_.target.disambiguation)
      .flatMap(_.annotation("system"))
  }

  def isPlainArrayRef(x: EirArrayReference): Boolean = {
    val system = arrayRefIsSystem(x)
    system.flatMap(_("alias").map(_.stripped)).contains("[]")
  }

  def implicitCast(
      goal: EirType,
      value: EirExpressionNode,
      requiresRef: Boolean
  )(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val valueTy = ctx.resolve(ctx.typeOf(value))

    (Find.tryClassLike(goal), Find.tryClassLike(valueTy)) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if b.isDescendantOf(a) =>
        ctx << ctx.nameFor(a) << "(" << value << ")"
      case (Some(a), Some(b)) if a.isPointer && b.isValueType =>
        structToTrait(value, b, noCopy = requiresRef)
      case _ => ctx << value
    }
  }

  def assignmentRhs(lhsTy: EirType, op: String, rhs: EirExpressionNode)(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    ctx << op << implicitCast(lhsTy, rhs, requiresRef = false)
  }

  override def visitAssignment(
      x: EirAssignment
  )(implicit ctx: CodeGenerationContext): Unit = {
    val lhsTy = ctx.resolve(ctx.typeOf(x.lval))

    x.lval match {
      case x: EirArrayReference if !isPlainArrayRef(x) => ctx << x << ";"
      case _ =>
        ctx << x.lval << assignmentRhs(lhsTy, x.op, x.rval) << ";"
    }
  }

  override def visitReturn(
      x: EirReturn
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "return" << x.expression << ";"
  }

  override def visitAwait(
      x: EirAwait
  )(implicit ctx: CodeGenerationContext): Unit = {
    val peeked = ctx.peekSentinel()
    if (peeked.nonEmpty) ???

    x.release match {
      case Some(_) =>
        val target = x.target
        val found = ctx.exprType(x.target)
        val tmp = ctx.temporary
        ctx << "([](" << ctx.typeFor(found, Some(x)) << tmp << ")" << "{"
        ctx << "auto" << "val" << "=" << tmp << fieldAccessorFor(
          found
        ) << "get()" << ";"
        ctx << tmp << fieldAccessorFor(found) << "release()" << ";"
        ctx << "return" << "val" << ";"
        ctx << "}" << ")(" << target << ")"
      case None => ctx << x.disambiguation
    }
  }

  // TODO implement this?
  private def escapeInterpString(s: String) = "\"" + s + "\""

  override def visitInterpolatedString(
      str: EirInterpolatedString
  )(implicit ctx: CodeGenerationContext): Unit = {
    def matchChild(x: EirExpressionNode) = {
      x match {
        // TODO is this right?? Idk...
        case x if x.disambiguation.isDefined =>
          ctx << "(([&](){ return " << visit(x.disambiguation.get) << "; })())"
        case x: EirLiteral if !x.value.startsWith("\"") =>
          ctx << escapeInterpString(x.value)
        case _ => ctx << x
      }
    }
    ctx << "("
    if (str.children.nonEmpty) {
      str.children.init.foreach(matchChild(_) << "+")
      matchChild(str.children.last)
    }
    ctx << ")"
  }

  override def visitPatternList(x: EirPatternList)(implicit
      ctx: CodeGenerationContext
  ): Unit = ()
  override def visitIdentifierPattern(x: EirIdentifierPattern)(implicit
      ctx: CodeGenerationContext
  ): Unit = ()
  override def visitExpressionPattern(x: EirExpressionPattern)(implicit
      ctx: CodeGenerationContext
  ): Unit = ()
  override def visitProxy(x: EirProxy)(implicit
      ctx: CodeGenerationContext
  ): Unit = ()
  override def visitTypeAlias(x: EirTypeAlias)(implicit
      ctx: CodeGenerationContext
  ): Unit = ()
  override def visitTupleMultiply(multiply: types.EirTupleMultiply)(implicit
      context: CodeGenerationContext
  ): Unit = ()
  override def visitConstantFacade(
      facade: EirConstantFacade
  )(implicit context: CodeGenerationContext): Unit =
    visit(facade.value)

  private def makeSentinel(
      ctx: CodeGenerationContext,
      all: Boolean,
      grouped: Boolean
  ): CodeGenerationContext.Sentinel = {
    val sentinel = "__armin__"
    val triplet =
      (all, sentinel, Option.when(grouped)(new mutable.Stack[String]))

    ctx << "{"
    ctx << "auto" << sentinel << "=" << "std::make_shared<hypercomm::sentinel>(999);"

    ctx.pushSentinel(triplet)
    triplet
  }

  @tailrec
  def canReuseSentinel(x: Option[EirNode]): Boolean = {
    x match {
      case Some(x: EirForLoop)   => x.annotation("overlap").isDefined
      case Some(_: EirAwaitMany) => true
      case Some(x: EirBlock)     => canReuseSentinel(x.parent)
      case _                     => false
    }
  }

  def findInplaceOpportunities(
      from: EirMember,
      arrArgs: List[Int],
      body: EirBlock
  )(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    if (!globals.enableInPlace) return

    val ours = from.counterpart
    val calls = Find.within[EirFunctionCall](
      body,
      x => {
        val theirs =
          x.target.disambiguation.to[EirMember].flatMap(_.counterpart)
        ours == theirs && {
          val theirArgs = x.args
            .flatMap(_.disambiguation)
            .flatMap(_.parent)
            .collect {
              case i: EirIdentifierPattern =>
                i.parent
                  .to[EirPatternList]
                  .map(_.patterns.indexOf(i))
                  .filter(_ >= 0)
            }
            .flatten
          arrArgs.forall(theirArgs.contains(_))
        }
      }
    )

    // TODO ensure that the args aren't (later) copied within the body

    for (call <- calls) {
      ctx.repack(call)

      Errors.warn(
        s"${Errors.contextualize(call)}: attempting to use inplace optimizations for call to ${from.name} (use `-fno-inplace` if you encounter issues)"
      )
    }
  }

  override def visitWhen(
      x: EirSdagWhen
  )(implicit ctx: CodeGenerationContext): Unit = {
    // TODO impl this
    val peeked = ctx.peekSentinel().filter(_ => canReuseSentinel(x.parent))
    val sentinel = peeked
      // only use an existing sentinel when its directly above us (for now)
      // todo eventually add a way to chain to parent
      .getOrElse({ makeSentinel(ctx, all = true, grouped = false) })
    val compound = x.patterns.length > 1
    if (compound && x.condition.isDefined) ???

    def encapsulate(types: List[EirResolvable[EirType]]): EirType = {
      if (types.nonEmpty) EirTupleType(None, types)
      else globals.unitType
    }

    val nPorts = x.patterns.length
    val com =
      sentinel._3
        .map(s => {
          val n = s"__com${s.length}__"
          s.push(n)
          n
        })
        .getOrElse("__com__")

    if (sentinel._3.isEmpty) ctx << "{"

    val set = "__vals__"

    ctx << "auto" << com << "=" << "ergoline::make_component(*this," << nPorts.toString << ","
    ctx << "[=](hypercomm::component::value_set&&" << set << ")" << "{"

    val quadruplets = x.patterns.map({
      case (symbol, _) =>
        val m = Find.namedChild[EirMember](ctx.proxy, symbol.qualifiedName.last)
        val f = assertValid[EirFunction](m.member)
        val declTys = f.functionArgs.map(_.declaredType).map(ctx.resolve)
        val tys = declTys.map(ctx.typeFor(_, Some(x)))
        val resolved = ctx.resolve(encapsulate(declTys))
        val mboxName = "this->" + GenerateProxies.mailboxName(ctx, f, tys)
        val arrayArgs =
          f.functionArgs.zipWithIndex
            .filter(x => isArray(ctx, ctx.typeOf(x._1)))
            .map(_._2)
        (m, mboxName, resolved, arrayArgs)
    })

    x.patterns.zipWithIndex.foreach({
      case ((_, patterns), i) =>
        val (m, mboxName, _, arrayArgs) = quadruplets(i)
        val name = s"__value${i}__"
        val ty = name.init + "type__"

        ctx << s"using" << ty << "=" << getMailboxType(mboxName) << ";"
        ctx << "auto" << name << "=" << s"hypercomm::value2typed<$ty>(std::move(" << set << "[" << i.toString << "]));"
        ctx << visitPatternDecl(
          ctx,
          patterns,
          name + "->value()",
          forceTuple = true
        ).split(n)

        if (arrayArgs.nonEmpty) {
          findInplaceOpportunities(m, arrayArgs, x.body)
        }
    })

    ctx.ignoreNext("{")
    ctx << x.body
    ctx << ");"

    x.patterns.zipWithIndex.foreach({
      case ((_, patterns), i) =>
        val (m, mboxName, resolved, arrayArgs) = quadruplets(i)
        val temp = ctx.temporary
        val declarations =
          visitPatternDecl(ctx, patterns, temp, forceTuple = true).split(n)
        val conditions =
          visitPatternCond(ctx, patterns, temp, Some(resolved)).mkString(" && ")

        val pred: Option[String] =
          Option.when(conditions.nonEmpty)({
            val name = s"__pred${i}__"
            val ty = name.init + "arg_type__"
            val constRef = s"const $ty&"
            ctx << "{"
            ctx << s"using" << ty << "=" << getMailboxType(mboxName) << ";"
            ctx << s"auto" << name << s"=ergoline::wrap_lambda<bool, $constRef>([=]($constRef $temp)" << "->" << "bool" << "{"
            ctx << declarations
            ctx << "return" << conditions
            ctx << x.condition.map(_ => "&&") << x.condition
            ctx << ";" << "});"
            name
          })

        ctx << s"$mboxName->put_request_to(" << pred.getOrElse("{},") << pred
          .map(_ => ",") << com << "," << i.toString << ");"

        pred.foreach(_ => ctx << "}")
    })

//    if (compound) {
//      ctx << "auto __compound__ ="
//      ctx << x.patterns.tail.indices.foldRight(s"__req0__")((i, s) => {
//        s"ergoline::join($s,__req${i + 1}__,nullptr,nullptr)"
//      })
//      ctx << ";"
//      val ty = "__compound_ty__"
//      ctx << s"using $ty = typename decltype(__compound__)::element_type;"
//      ctx << s"${sentinel._2}.put(std::static_pointer_cast<typename $ty::parent_t>(__compound__)," << s"[=](typename $ty::value_t&& __value__)" << "{"
//    } else {
//      val ty = "__req0_val__"
//      ctx << s"${sentinel._2}.put(__req0__," << s"[=]($ty&& __value__)" <<
//    }

    if (sentinel._3.isEmpty) {
      ctx << sentinel._2 << "->" << "expect_all" << "(" << com << ");"
      ctx << "this->activate_component(" << com << ");"
      ctx << "}"
    }

    if (peeked.isEmpty) popSentinel(sentinel)
  }

  override def visitSlice(x: EirSlice)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    visit(x.disambiguation)
  }

  def popSentinel(
      sentinel: CodeGenerationContext.Sentinel
  )(implicit ctx: CodeGenerationContext): Unit = {
    sentinel._3 match {
      case Some(s) if s.nonEmpty =>
        ctx << sentinel._2 << "->"
        ctx << "expect_" + (if (sentinel._1) "all" else "any") << "("
        s.init.foreach(com => ctx << com << ",")
        ctx << s.last << ");"

        s.foreach(com => ctx << "this->activate_component(" << com << ");")
      case _ =>
    }
    ctx << s"${sentinel._2}->suspend();"

    GenerateProxies.updateLocalityContext(ctx)

    ctx << "}"
    ctx.popSentinel(sentinel)
  }

  override def visitAwaitMany(
      x: EirAwaitMany
  )(implicit ctx: CodeGenerationContext): Unit = {
    val peeked = ctx.peekSentinel().filter(_ => canReuseSentinel(x.parent))
    val sentinel = peeked.getOrElse({
      makeSentinel(ctx, all = x.waitAll, grouped = true)
    })
    x.children.foreach(visit)
    if (peeked.isEmpty) popSentinel(sentinel)
  }
}
