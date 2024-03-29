package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.literals.{
  EirBooleanLiteral,
  EirIntegerLiteral,
  EirLiteral,
  EirLiteralSymbol,
  EirStringLiteral,
  EirUnitLiteral
}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.LazyCodeBlock
import edu.illinois.cs.ergoline.passes.GenerateProxies.{
  indicesName,
  mailboxName,
  updateLocalityContext,
  valueHandlerFor
}
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{
  EirPlaceholder,
  EirResolvable,
  EirTemplateFacade,
  Find
}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{
  RichOption,
  RichResolvableTypeIterable
}
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike
import edu.illinois.cs.ergoline.util.{Errors, assertValid, onLeftSide}
import edu.illinois.cs.ergoline.util
import edu.illinois.cs.ergoline.util.Errors.Limitation

import java.io.File
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends EirVisitor[CodeGenerationContext, Unit] {

  case class CppNode(s: String)
      extends EirExpressionNode
      with EirEncloseExempt {
    override var parent: Option[EirNode] = None
    override def children: Iterable[EirNode] = Nil
    override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
  }

  var visited: List[EirNode] = Nil
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit =
    (ctx, x) => visit(x)(ctx)

  object GenCppSyntax {
    implicit class RichEirNode(self: EirNode) {
      def isSystem: Boolean = util.isSystem(self)

      def memberParent: Option[EirNode] = {
        asMember(self).flatMap(_.parent)
      }

      def systemParent: Boolean = {
        self.memberParent.exists(_.isSystem)
      }
    }

    implicit class RichEirType(self: EirType) {
      def isReconstructible(implicit ctx: CodeGenerationContext): Boolean = {
        // TODO refine this with @system(reconstructible=true)
        // TODO eliminate the check for EirTemplateArgument here
        // TODO fix reconstruction of tuple types!
        self match {
          case _: EirTemplateArgument | _: EirTupleType          => false
          case _: EirProxy | EirTemplatedType(_, _: EirProxy, _) => false
          case _                                                 => !self.isPointer && !self.isSystem
        }
      }

      def isPointer(implicit ctx: CodeGenerationContext): Boolean = {
        self match {
          case t: EirTemplateArgument => Option(ctx)
              .zip(t.parent.to[EirSpecializable])
              .map { case (ctx, s) => zipWithSpecializations(Seq(s))(ctx) }
              .exists(pairs => {
                pairs forall { case (s, sp) =>
                  val idx = s.templateArgs.indexOf(t)
                  ctx.resolve(sp(idx)).isPointer
                }
              })
          case _: EirLambdaType => true
          case _ => Find
              .tryClassLike(self)
              .exists(!_.isValueType)
        }
      }

      def isSystem: Boolean =
        Find.tryClassLike(self).flatMap(_.annotation("system")).nonEmpty

      def isTrait: Boolean = Find.asClassLike(self).isInstanceOf[EirTrait]
    }

    implicit class RichEirResolvable[T <: EirNode](self: EirResolvable[T]) {
      def isTransient: Boolean = {
        val x = Find.uniqueResolution[EirNode](self)
        x match {
          case t: EirTupleType => t.children.exists(_.isTransient)
          // TODO use specialization?
          case _: EirTemplateArgument => false
          case _: EirLambdaType       => false
          case x: EirClassLike        => x.isTransient
          case t: EirType             => Find.asClassLike(t).isTransient
        }
      }
    }
  }

  override def fallback(implicit ctx: CodeGenerationContext): Matcher = {
    case EirPlaceholder(_, Some(arg: EirFunctionArgument)) =>
      ctx << ctx.nameFor(arg)
    case CppNode(s) =>
      val lines = s.split(";").map(_.trim).filterNot(_.isEmpty)
      lines.init.foreach(l => ctx << (l + ";"))
      lines.lastOption.foreach(l => {
        ctx << l << Option.when(s.trim.endsWith(";"))(";")
      })
  }

  import GenCppSyntax.{RichEirNode, RichEirResolvable, RichEirType}

  val enablerName = "__ENABLE__"

  def forwardDecl(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (x.isSystem) return

    x.predicate match {
      case Some(_) =>
        templateArgsOf(x) match {
          case Nil => ctx << "template" << "<"
          case args =>
            ctx.ignoreNext(">")
            ctx << visitTemplateArgs(args)(ctx)
            ctx << ","
        }
        ctx << "typename" << enablerName << "=" << "void" << ">"
      case None => visitTemplateArgs(x)
    }

    ctx << "struct" << declNameFor(x) << ";"

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
    ) foreach { case (p, types) =>
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
        val msgName = GenerateProxies.msgName
        ctx << "struct" << mainName << ":" << "public" << ("CBase_" + mainName) << "{"
        ctx << mainName << "(" << "CkArgMsg*" << msgName << ")" << "{"
        val concreteSingletons = zipWithSpecializations(
          ProxyManager.singletons.filterNot(_.isAbstract)
        )

        concreteSingletons foreach { case (p, types) =>
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

  var corePupables: Set[String] = Set(
    "hypercomm::future_port",
    "hypercomm::port_opener",
    "hypercomm::null_combiner",
    "hypercomm::inter_callback",
    "hypercomm::forwarding_callback<CkArrayIndex>"
  )

  val localityPupables: Seq[String] = Seq(
    "hypercomm::vector_section",
    "hypercomm::reduction_port",
    "hypercomm::broadcaster<CkArrayIndex,"
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

    val regdIdxs = ProxyManager.registeredIndices()
    regdIdxs foreach { x =>
      localityPupables foreach (y => {
        ctx << "hypercomm::enroll<" << y
        val numLess = y.count(_ == '<')
        ctx << Option.unless(numLess > 0 && y.count(_ == '>') <= numLess)(
          "<"
        ) << ctx.typeFor(x, global) << ">>" << "()" << ";"
      })
    }

    var anyObjects = false
    def makeEnroll(x: EirSpecializable): Unit = {
      val name = ctx.nameFor(x, global)
      val objectType = x match {
        case c: EirClass => c.objectType
        case _           => false
      }
      anyObjects = anyObjects || objectType

      ctx << "hypercomm::enroll<" << name << ">();"

      if (objectType) {
        ctx << "hypercomm::enroll<ergoline::typed_singleton<" << name << ">>();"
      }
    }

    puppables.foreach(x => {
      if (x.templateArgs.isEmpty) {
        makeEnroll(x)
      } else {
        checked(x).foreach(y => {
          val sp = ctx.specialize(x, y)
          makeEnroll(x)
          ctx.leave(sp)
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
    )(ctx) foreach { case (p, types) =>
      val (ty, name) = ("int", counterFor(p, types, global)(ctx))
      ctx << "CpvInitialize(" << ty << "," << name << ");"
      ctx << "CpvAccess(" << name << ") = 0;"
    }

    Processes.modules.foreach(mod => {
      ctx << s"_register$mod();"
    })

    if (anyObjects) {
      ctx << "ergoline::setup_singleton_module();"
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
        val dim =
          arrayDim(ctx, ctx.typeOf(x.target)).to[EirIntegerLiteral].map(_.value)

        dim match {
          case Some(1) => ctx << "(int)" << x.target << "->shape[0]"
          case Some(n) if n > 0 =>
            ctx << "std::tuple_cat(" << x.target << "->shape)" // TODO is this tested?
          case _ => Errors.unreachable()
        }
      case _ =>
        val targetTy: EirType = ctx.exprType(x.target)
        val isEntryOnly = found match {
          case m: EirMember => m.isEntryOnly
          case _            => false
        }
        val targetsSelf = x.target match {
          case s: EirSymbol[_] => s.qualifiedName match {
              case "self" :: Nil => true
              case _             => false
            }
          case _ => false
        }

        (if (targetsSelf && isEntryOnly) {
           ctx << ctx.currentProxySelf
         } else {
           ctx << x.target
         }) << fieldAccessorFor(targetTy)(Some(x.isStatic)) << ctx.nameFor(
          found
        )
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
      case Some(n: EirIntegerLiteral) =>
        (0 until n.value).map(idx => s"$curr[$idx]")
      case _ => Errors.unreachable()
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

  def structToTrait(node: EirNode, ty: EirType, noCopy: Boolean)(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val name = ctx.nameFor(ty, Some(node))
    if (noCopy) {
      ctx << "std::shared_ptr<" << name << ">(std::shared_ptr<void>{},&" << node << ")"
    } else {
      ctx << "std::make_shared<" << name << ">(" << node << ")"
    }
  }

  def visitCallArgument(t: (EirExpressionNode, EirFunctionArgument))(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val requiresRef = t._1 match {
      case arg: EirCallArgument => arg.isRef
      case _                    => false
    }

    val (ours, theirs) =
      (ctx.typeOf(t._1), Find.uniqueResolution[EirType](t._2.declaredType))
    val shouldDeref = ours.isPointer && t._2.isReference

    ctx << Option.when(shouldDeref)("(*(") << implicitCast(
      (t._1, ours),
      theirs,
      requiresRef
    ) << Option.when(shouldDeref)("))")
  }

  def visitArguments(
      fc: Option[EirFunctionCall],
      disambiguation: Option[EirNode],
      rawArgs: ArgumentList
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    visitArguments(fc, disambiguation, rawArgs._1 ++ rawArgs._2)
  }

  private def prepareType(
      ty: EirType
  )(implicit ctx: CodeGenerationContext): EirType = {
    val place = Orchestrate.placeholder
    val deref = CheckTypes.stripReference(ty)
    deref match {
      case t: EirTemplatedType
          if place == Find.uniqueResolution[EirType](t.base) =>
        Find.uniqueResolution[EirType](t.args.head)
      case _ => deref
    }
  }

  // TODO direct use of this function should be considered suspect!
  def visitArguments(
      fc: Option[EirFunctionCall],
      disambiguation: Option[EirNode],
      args: List[EirExpressionNode]
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    val member: Option[EirMember] = asMember(disambiguation)
    // TODO ( make these checks more robust )
    val isAsync = member.exists(_.hasAnnotation("async"))
    val isProxyMember = member.exists(_.hasAnnotation("proxy"))
    val isDoneInserting = member.exists(_.name == "doneInserting")
    val isConstructorOrInserter = (m: EirMember) => {
      m.isConstructor || (m.name == "insert")
    }
    val isMulticast = member.flatMap(_.parent).to[EirProxy].exists(_.isSection)
    val isUnitLocal = member.exists(x => x.isLocal && !memberHasArgs(x))

    val shouldPack = member.flatMap {
      case _
          if isDoneInserting || isMulticast || isUnitLocal || isProxyMember =>
        None
      case m @ EirMember(Some(_: EirProxy), _, _) => Some(
          (
            !(isAsync || isConstructorOrInserter(m)),
            if (m.isMailbox) {
              mailboxName(ctx, m)._2
            } else {
              args
                .map(ctx.exprType)
                .map(prepareType)
                .map(ctx.typeFor(_, fc))
            }
          )
        )
      // TODO this should be a local call (that does not involve packing!)
      case m: EirMember =>
        Option.when(args.nonEmpty && m.isEntryOnly)((false, Nil))
    }

    // TODO add support for expansions
    val theirs: List[EirFunctionArgument] =
      member.orElse(disambiguation) match {
        // TODO this should drop args for new~!
        case Some(_ @EirMember(_, f: EirFunction, _)) =>
          f.functionArgs ++ f.implicitArgs
        case Some(f: EirFunction) => f.functionArgs ++ f.implicitArgs
        case _                    => Nil
      }

    shouldPack match {
      case Some((false, _)) =>
        if (fc.exists(ctx.shouldRepack)) ctx << "ergoline::repack("
        else ctx << "hypercomm::pack("
      case Some((true, tys)) =>
        ctx << "hypercomm::make_typed_value<"
        if (tys.length == 1) {
          ctx << tys.head
        } else {
          ctx << "std::tuple<" << (tys, ",") << ">"
        }
        ctx << ">("
        if (tys.length >= 2) ctx << "std::forward_as_tuple("
      case None =>
    }

    if (isAsync) {
      ctx << ctx.temporary
      if (args.nonEmpty) ctx << ","
    }

    if (theirs.length == args.length) {
      val zipped = args.zip(theirs)

      if (zipped.nonEmpty) {
        zipped.init.foreach(pair => {
          visitCallArgument(pair)(ctx)
          ctx << ","
        })

        visitCallArgument(zipped.last)(ctx)
      }
    } else {
      ctx << (args, ",")
    }

    ctx << shouldPack.map {
      case (true, tys) if tys.length >= 2 => "))"
      case _                              => ")"
    }
  }

  def visitCallback(target: EirExpressionNode, isReduction: Boolean)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    target match {
      case s @ EirScopedSymbol(base, _) =>
        val found = asMember(disambiguate(ctx, target) match {
          case x: EirLambdaExpression if ctx.isMailbox(x) =>
            Option(s.pending).to[EirExpressionNode].flatMap(_.disambiguation)
          case x => Some(x)
        })
        val baseType = base.foundType
        val proxy = baseType.flatMap(Find.tryClassLike).to[EirProxy]
        if (proxy.isDefined && found.exists(_.isEntry)) {
          ctx << "CkCallback("
          ctx << epIndexFor(baseType.get, found.get, Some(target), None)
          ctx << "," << base << ")"
        } else {
          Errors.expectedCallback(target)
        }
      case EirSymbol(_, "exit" :: _)
          if (globals.ergolineModule == disambiguate(ctx, target).parent) =>
        ctx << "CkCallback(CkCallback::ckExit)"
      case expr @ EirFunctionCall(_, symbol, Nil, _)
          if globals.isResumeThread(disambiguate(ctx, symbol)) => ctx << expr
      case _ => Errors.expectedCallback(target)
    }
  }

  def visitReducer(
      _target: EirExpressionNode
  )(implicit ctx: CodeGenerationContext): Unit = {
    val target = asMember(disambiguate(ctx, _target))
    val annotation =
      target.flatMap(_.annotation("system")).flatMap(_("reducer"))
    annotation match {
      case Some(x: EirStringLiteral) => ctx << x.strip()
      case _                         => Errors.expectedReducer(_target)
    }
  }

  // TODO all uses of this should be considered legacy/suspect!
  def flattenArgs(args: ArgumentList): List[EirExpressionNode] = {
    args._1.map(_.expr) ++ args._2
  }

  def handleOptionMember(implicit
      ctx: CodeGenerationContext,
      m: EirMember,
      fc: EirFunctionCall,
      args: List[EirExpressionNode]
  ): Unit = {
    val base = fc.target
    val apl = m.name == "apply"
    val opt = base match {
      case s: EirScopedSymbol[_] => s.target
      case _ if apl              => base
      case _                     => Errors.unreachable()
    }
    val rsv = {
      if (apl) {
        opt match {
          case EirSpecializedSymbol(_, base, tys) => EirTemplatedType(
              None,
              assertValid[EirResolvable[EirType]](base),
              tys
            )
          case _ =>
            val option = globals.optionType
            val spec = CheckTypes.inferSpecialization(option, fc)(ctx.tyCtx)
            spec
              .map(s => EirTemplatedType(None, option, s.types))
              .getOrElse(Errors.missingType(opt))
        }
      } else {
        // TODO this overly specializes! e.g., option<A> ==> option<int>
        ctx.typeOf(opt)
      }
    }
    val headTy = ctx.resolve(rsv) match {
      case t: EirTemplatedType if t.args.length == 1 => ctx.resolve(t.args.head)
      case _                                         => Errors.unreachable()
    }
    val (ref, ty) = headTy match {
      case EirReferenceType(_, t) => (true, ctx.resolve(t))
      case t                      => (false, t)
    }
    val ptr = ty.isPointer
    val astr = Option.unless(ptr)("*")
    def wrap(ty: EirType): Option[String] = Option.unless(ty.isPointer)(
      (if (ref) "std::shared_ptr" else "std::make_shared") + s"<${ctx
        .typeFor(ty, Some(base))}>"
    )
    m.name match {
      case "get"                   => ctx << "(" << astr << opt << ")"
      case "apply" if args.isEmpty => ctx << "nullptr"
      case "apply" if args.nonEmpty =>
        if (ptr) ctx << args.head
        else {
          ctx << wrap(ty) << "("
          if (ref && !ptr) {
            ctx << "&(" << args.head << "), [](void*){})"
          } else {
            ctx << args.head << ")"
          }
        }
      case "nonEmpty" | "isEmpty" =>
        ctx << "(" << opt << (if (m.name == "nonEmpty") "!="
                              else "==") << "nullptr" << ")"
      case "getOrElse" =>
        ctx << "(" << opt << "?" << astr << opt << ":" << args.head << ")"
      case "map" | "flatMap" =>
        val wrapped = Option
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
    val ty = CheckTypes.stripReference(ctx.resolve(rsv)) match {
      case t: EirTemplatedType if t.args.length == 1 => ctx.resolve(t.args.head)
      case _                                         => Errors.unreachable()
    }
    m.name match {
      case "apply" => ctx.proxy match {
          case Some(_) => ctx << ctx.currentProxySelf << "->make_future()"
          case None => ctx << "ergoline::make_future(" << {
              globals.implicitProxyName
            } << ")"
        }
      case "set" =>
        ctx << fut << ".set(hypercomm::pack_to_port({}," << (args, ",") << "))"
      case "get" =>
        val futureName = "f"
        val retTy = ctx.typeFor(ty, Some(base))
        ctx << "(([&](const hypercomm::future&" << futureName << ")" << "->" << retTy << "{"
        ctx << "auto cb = std::make_shared<hypercomm::resuming_callback<" << retTy << ">>();"
        ctx << ctx.currentProxySelf << "->request_future(" << futureName << ", cb);"
        ctx << "cb->wait();"
        updateLocalityContext(ctx)
        ctx << "return cb->value();"
        ctx << "})(" << fut << "))"
      case "then" =>
        val futureName = "f"
        ctx << "(([&](const hypercomm::future&" << futureName << ")" << "{"
        ctx << "auto cb = " << {
          args.headOption.foreach(x => {
            visitCallback(x, isReduction = false)(ctx)
          })
        } << ";"
        ctx << ctx.currentProxySelf << "->request_future(" << futureName << ", std::make_shared<hypercomm::inter_callback>(cb));"
        ctx << "})(" << fut << "))"
      case _ => ???
    }
  }

  def isOption(t: EirType): Boolean = t match {
    case t: EirClass =>
      (t.name == "option") && (t.parent == globals.ergolineModule)
    case _ => false
  }

  def isOption(t: Option[EirNode]): Boolean = t.to[EirType].exists(isOption)

  def isFuture(t: EirType): Boolean = t match {
    case t: EirClass => (t.name == "future") && (t.parent == globals.ckModule)
    case _           => false
  }

  def isFuture(t: Option[EirNode]): Boolean = t.to[EirType].exists(isFuture)

  def isMailbox(t: EirType): Boolean = t match {
    case t: EirClass => (t.name == "mailbox") && (t.parent == globals.ckModule)
    case _           => false
  }

  def isMailbox(t: Option[EirNode]): Boolean = t.to[EirType].exists(isMailbox)

  type ArgumentList =
    (List[EirCallArgument], List[EirSymbol[EirImplicitDeclaration]])

  @tailrec
  def stripSection(
      target: EirExpressionNode,
      getProxy: Boolean = false
  ): EirExpressionNode = {
    target match {
      case EirScopedSymbol(target, _) => stripSection(target, getProxy)
      case EirArrayReference(_, target, args) =>
        if (args.length == 1)
          stripSection(if (getProxy) target else args.head, getProxy)
        else ??? // TODO add support for zipping iterators?
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
        ctx << ctx.currentProxySelf << "->local_contribution(" << "ergoline::conv2section(" << stripSection(
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
        ctx << "ergoline::contribute(" << ctx.currentProxySelf << "," << {
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

  def memberHasArgs(m: EirMember): Boolean = m.member match {
    case f: EirFunction => f.functionArgs.nonEmpty
    case _              => false
  }

  def epIndexFor(
      proxy: EirType,
      member: EirMember,
      usage: Option[EirNode],
      sp: Option[EirSpecialization]
  )(implicit
      ctx: CodeGenerationContext
  ): String = {
    epIndexFor(
      proxy,
      member,
      usage,
      hasArgs = memberHasArgs(member),
      sp
    )
  }

  def epIndexFor(
      ty: EirType,
      member: EirMember,
      usage: Option[EirNode],
      hasArgs: Boolean,
      sp: Option[EirSpecialization] = None
  )(implicit
      ctx: CodeGenerationContext
  ): String = {
    val proxy = assertValid[EirProxy](Find.asClassLike(ty))
    val isAsync = member.annotation("async").nonEmpty
    val isAbstract = proxy.isAbstract
    val qualifications = usage.map(qualificationsFor(proxy, _)).getOrElse(Nil)
    val args =
      if (templateArgsOf(proxy).nonEmpty) ty match {
        case ty: EirTemplatedType => ty.args
        case _                    => Errors.missingSpecialization(proxy)
      }
      else {
        Nil
      }
    (qualifications ++ {
      if (member.isMailbox) {
        Seq(
          indicesName(proxy),
          s"${mailboxName(ctx, member)._1}idx__"
        )
      } else {
        val spec = sp
          .map(_.types)
          .filterNot(_.isEmpty)
          .map(templateArgumentsToString(_, usage)(ctx))
          .getOrElse("")
        if (isAbstract) {
          // TODO ( this is NOT robust if usage is wrong/unrepeatable )
          Seq({
            val inner = ctx.makeSubContext()
            inner << usage << "." << GenerateProxies.abstractIndexOf(member)(
              inner
            )
            inner.toString
          })
        } else {
          Seq(
            s"CkIndex_${proxy.baseName}" + (
              if (args.nonEmpty)
                templateArgumentsToString(ctx, Some(proxy), args, usage)
              else ""
            ),
            ctx.nameFor(member) + spec + "(" + {
              Option.when(hasArgs || isAsync)("nullptr").getOrElse("")
            } + ")"
          )
        }
      }
    }).mkString("::")
  }

  def visitMulticast(
      proxy: EirProxy,
      member: EirMember,
      fc: EirFunctionCall,
      args: ArgumentList
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    val hasArgs = (args._1.size + args._2.size) != 0
    val target = fc.target

    {
      stripSection(target, getProxy = true) match {
        case s: EirSymbol[_] if CheckTypes.isSelf(s) =>
          ctx << ctx.currentProxySelf << "->broadcast("
        case s =>
          ctx << "hypercomm::broadcast_to(hypercomm::make_proxy(" << s << "),"
      }
    } << "ergoline::conv2section(" << stripSection(
      target
    ) << ")" << "," << {
      epIndexFor(proxy, member, Some(fc), hasArgs)
    } << ", hypercomm::pack_to_port({}" << Option.when(hasArgs)(",") << {
      visitArguments(Some(fc), Some(member), args)
    } << ")" << ")"
  }

  def formatSystemArgs(
      fmt: String,
      base: EirExpressionNode,
      args: ArgumentList,
      pairs: Map[EirTemplateArgument, EirResolvable[EirType]]
  )(implicit ctx: CodeGenerationContext): Unit = {
    val placeholderPtn = "\\{([_a-zA-Z\\d]+)}".r
    val matches = placeholderPtn.findAllMatchIn(fmt).toList
    val startPos =
      (i: Int) => if (i >= matches.length) fmt.length else matches(i).start
    val baseArg = EirCallArgument(base, isRef = false)(None)
    val gathered = (baseArg +: args._1) ++ args._2
    ctx << fmt.substring(0, startPos(0))
    matches.zipWithIndex.foreach { case (matches, i) =>
      val (m, end) = (matches.group(1), matches.end)
      if (m.forall(_.isDigit)) {
        // TODO use visit call argument here?
        ctx << gathered(m.toInt)
      } else {
        pairs.keys.find(m == _.name).map(pairs(_)) match {
          case Some(ty) => ctx << ctx.typeFor(ty, Some(base))
          case None     => Errors.unableToResolve(m)
        }
      }
      ctx << fmt.substring(end, startPos(i + 1))
    }
  }

  def handleMailboxMember(implicit
      ctx: CodeGenerationContext,
      m: EirMember,
      fc: EirFunctionCall,
      disambiguated: EirNode
  ): Unit = {
    val isApply = m.name == "apply"
    val first = Option(fc.target).to[EirScopedSymbol[EirNamedNode]]
    val target = first
      .filter(_ => isApply)
      .orElse(first.map(_.target).to[EirScopedSymbol[EirNamedNode]])
    val proxy =
      target.flatMap(_.target.foundType).flatMap(Find.tryClassLike).to[EirProxy]
    val member = asMember(
      if (isApply)
        target.map(_.pending).to[EirExpressionNode].flatMap(_.disambiguation)
      else target.flatMap(_.disambiguation)
    )
    if (proxy.isEmpty || member.isEmpty) Errors.invalidAccess(fc, m)
    val idx = proxy.zip(member).map { case (p, m) =>
      epIndexFor(p, m, target.orElse(Some(fc)), None)
    }
    m.name match {
      case "apply" =>
        ctx << (proxy match {
          case Some(p: EirProxy) if p.isCollective || p.isSection =>
            "ergoline::broadcast_value"
          case Some(_: EirProxy) => "hypercomm::interceptor::send_async"
          case n                 => Errors.incorrectType(n.orNull, classOf[EirProxy])
        })
        ctx << "(" << target.map(
          _.target
        ) << "," << idx << "," << visitArguments(
          Some(fc),
          member,
          (fc.args, Nil)
        ) << ")"
      case "requirePost" =>
        ctx << "(([&](void) { CkAssert(" << fc.args.map(_.expr) << ");"
        ctx << ctx.currentProxySelf << "->manual_mode(" << idx << ");" << "})())"
      case "post" =>
        val bufName = "__buffer__"
        ctx << "(([&](void) { auto" << bufName << " = " << fc.args.map(
          _.expr
        ) << ";"
        ctx << ctx.currentProxySelf << "->post_buffer(" << idx << ","
        ctx << "std::shared_ptr<void>(" << bufName << "," << bufName << "->data())"
        ctx << "," << bufName << "->size());" << "})())"
      case _ => Errors.unreachable()
    }
  }

  def visitSystemCall(implicit
      ctx: CodeGenerationContext,
      fc: EirFunctionCall,
      disambiguated: EirNode,
      args: ArgumentList
  ): Unit = {
    val target = fc.target
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
      system("alias").map(_.strip()).getOrElse(ctx.nameFor(disambiguated))
    val resWrapper = system("applyResult").map(_.strip()).map(_ + "(")
    val format = system("format").map(_.strip())
    ctx << resWrapper
    disambiguated match {
      case _ if format.nonEmpty =>
        val types = Option(fc.types)
          .filterNot(_.isEmpty)
          .orElse(
            Option(target).to[EirSpecializedSymbol[_]].map(_.types)
          )
        val pairs = types
          .map(tys => {
            disambiguated match {
              case sp: EirSpecializable => sp.templateArgs.zip(tys)
              case _                    => Nil
            }
          })
          .map(_.toMap)
          .getOrElse(Map())
        format.foreach(formatSystemArgs(_, base, args, pairs))
      case _ if operator =>
        val flattened = flattenArgs(args)
        if (flattened.isEmpty) {
          assert(name.startsWith(globals.unaryPrefix))
          ctx << "(" << name.substring(
            globals.unaryPrefix.length
          ) << base << ")"
        } else {
          assert(flattened.size == 1)
          ctx << "(" << base << name << flattened.head << ")"
        }
      case _ if name == "ergoline::broadcast_singleton" =>
        ctx << name << "<" << {
          (target match {
            case s: EirSpecialization => s.types
            case _                    => fc.types
          }).headOption
            .to[EirExpressionNode]
            .map(disambiguate(ctx, _))
            .map(ctx.nameFor(_, Some(fc)))
        } << ">(" << {
          visitCallback(flattenArgs(args).last, isReduction = false)
        } << ")"
      case m: EirMember if m.name == "toString" =>
        // TODO ( make this more robust )
        ctx << s"ergoline::to_string(" << (target match {
          case s: EirScopedSymbol[_] => s.target
          case _                     => ???
        }) << ")"
        assert(fc.args.isEmpty)
      case m: EirMember if isArray(ctx, m.base) && m.name == "size" =>
        ctx << target // bypass args for size!
      case m @ EirMember(Some(p: EirProxy), _, _) if m.name == "contribute" =>
        visitContribute(p, target, args)
      case m @ EirMember(Some(p: EirProxy), _, _) if p.isSection =>
        visitMulticast(p, m, fc, args)
      case m @ EirMember(Some(p: EirProxy), _, _)
          if m.name == "index" || m.name == "parent" =>
        name match {
          case "index" => ctx << selfIndex(ctx)
          case "parent" => ctx << s"(CProxy_${p.baseName}(" << base << {
              p.collective match {
                case Some("group" | "nodegroup")      => ".ckGetGroupID()))"
                case Some(s) if s.startsWith("array") => ".ckGetArrayID()))"
                case _                                => ???
              }
            }
          case _ => ???
        }
      case m: EirMember if isMailbox(disambiguated.parent) =>
        handleMailboxMember(ctx, m, fc, disambiguated)
      case m: EirMember if isOption(disambiguated.parent) =>
        handleOptionMember(ctx, m, fc, flattenArgs(args))
      case m: EirMember if isFuture(disambiguated.parent) =>
        handleFutureMember(ctx, m, target, flattenArgs(args))
      case _: EirMember if static =>
        ctx << s"$name(" << {
          val baseArg = EirCallArgument(base, isRef = false)(None)
          visitArguments(
            Some(fc),
            Some(disambiguated),
            (baseArg +: args._1, args._2)
          )
        } << ")"
      case EirMember(_, f: EirFunction, _) if cast =>
        ctx << s"((" << ctx.typeFor(f.returnType) << ")" << base << ")"
      case m: EirMember =>
        if (name == "apply") {
          ctx << base << s"(" << visitArguments(
            Some(fc),
            Some(disambiguated),
            args
          ) << ")"
        } else {
          val fqnOrDot = fieldAccessorFor(ctx.exprType(base))(Some(m.isStatic))
          ctx << invOp << base << s"$fqnOrDot$name(" << visitArguments(
            Some(fc),
            Some(disambiguated),
            args
          ) << ")"
        }
      case _: EirFunction if name == "launch_" =>
        val fn = Find.ancestors(fc).collectFirst { case x: EirFunction => x }
        ctx << "hypercomm::tasking::launch<" << fn.map(
          GenerateStencil.taskNameFor
        ) << ">("
        ctx << "ergoline::workgroup()," << visitArguments(
          Some(fc),
          Some(disambiguated),
          args
        )
        ctx << ")"
      case f: EirFunction if name == "CkPrintf" || name == "::CkAbort" =>
        val endl = if (f.name == "println") "\\n" else ""
        ctx << name << "(\"%s" << endl << "\"," << "(" << {
          visitArguments(Some(fc), Some(disambiguated), args)
        } << ")" << ".c_str())"
      case _ => ctx << s"($name(" << visitArguments(
          Some(fc),
          Some(disambiguated),
          args
        ) << "))"
    }
    ctx << resWrapper.map(_ => ")")
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

  def isFunctor(expr: EirExpressionNode, clarifier: EirNode): Boolean = {
    clarifier match {
      case m: EirMember if m.name == "apply" =>
        // TODO this assumption may be weak!
        !expr.isInstanceOf[EirScopedSymbol[_]]
      case _ => false
    }
  }

  private def ckLocalBranch(proxy: EirProxy, expr: EirExpressionNode)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    ctx << "((" << proxy.baseName << "*)("
    val idx = expr match {
      case x: EirArrayReference =>
        ctx << x.target
        Some(x.args)
      case _ =>
        ctx << expr
        None
    }
    ctx << ".ckLocalBranch())->lookup("
    idx match {
      case Some(is) => makeArrayIndex(is)
      // TODO ( this is unreliable if expr is unrepeatable )
      case None => ctx << "(" << expr << ".ckGetIndex())"
    }
    ctx << "))"
  }

  override def visitFunctionCall(
      x: EirFunctionCall
  )(implicit ctx: CodeGenerationContext): Unit = {
    val disambiguated = disambiguate(ctx, x.target)
    val member = asMember(disambiguated)
    val static = member.map(_.isStatic)
    val systemParent = disambiguated.systemParent
    val target = x.target match {
      case s: EirScopedSymbol[_] => Some(s.target)
      case _                     => None
    }
    val implicits = CheckTypes.getImplicitArgs(disambiguated) map { i =>
      // TODO enforce implicitness
      val symbol = EirSymbol[EirImplicitDeclaration](Some(x), List(i.name))
      symbol.foundType = Some(ctx.typeOf(i.declaredType))
      symbol
    }
    val s = member collect {
      case m @ EirMember(_, s: EirSpecializable, _)
          if m.isEntry && s.templateArgs.nonEmpty => s
    }
    val sp = s
      .flatMap { s =>
        val counterpart = member.flatMap(_.counterpart)
        // a recursive call -- so reuse the args! this is a hack :')
        if (counterpart.exists(Find.ancestors(x).contains(_))) {
          Some(ctx.tyCtx.synthesize(s.templateArgs))
        } else {
          val inner = new TypeCheckContext(Some(ctx.tyCtx))
          val spec = CheckTypes.inferSpecialization(s, x)(inner)
          assert(spec.nonEmpty)
          spec
        }
      }
      .getOrElse(x)
    val isAsync = disambiguated.hasAnnotation("async")
    val isProxy = disambiguated.hasAnnotation("proxy")
    val isLocal = member.exists(_.isLocal)
    if (disambiguated.isSystem) {
      ctx << visitSystemCall(
        ctx,
        x,
        disambiguated,
        (x.args, implicits)
      )
    } else {
      val proxy = member.flatMap(_.parent).to[EirProxy]
      val functor = isFunctor(x.target, disambiguated)
      if (isAsync) {
        val retTy = disambiguated match {
          case EirMember(_, f: EirFunction, _) => ctx.resolve(f.returnType)
          case f: EirFunction                  => ctx.resolve(f.returnType)
          case _                               => Errors.missingType(disambiguated)
        }
        ctx << "(([&](){" << ctx.typeFor(
          retTy
        ) << ctx.temporary << "=" << ctx.currentProxySelf << "->make_future()" << ";"
      }
      val isPointer = x.target match {
        // TODO make this more robust
        case s: EirSymbol[_] => ctx.resolve[EirNode](s) match {
            case _: EirDeclaration                  => true
            case EirMember(_, _: EirDeclaration, _) => true
            case _: EirFunctionArgument             => true
            case _                                  => false
          }
        case _ => false
      }
      if (systemParent) {
        assert(!isPointer) // TODO can this be relaxed?

        ctx << ctx.nameFor(disambiguated, Some(x)) << "("
        if (!static.contains(true)) {
          ctx << (x.target match {
            case y: EirScopedSymbol[_] => y.target
            case y                     => y
          }) << Option.unless(x.args.isEmpty && implicits.isEmpty)(",")
        }
        ctx << visitArguments(
          Some(x),
          Some(disambiguated),
          (x.args, implicits)
        ) << ")"
      } else {
        if (functor) {
          ctx << x.target << x.foundType.map(fieldAccessorFor(_)(static)) << {
            ctx.nameFor(disambiguated)
          }
        } else if (isProxy) {
          val targetType = target.map(ctx.exprType)
          val (proxyType, sp) = targetType match {
            case Some(EirTemplatedType(_, t: EirProxy, args)) =>
              (Some(t), Some(args))
            case Some(t: EirProxy) => (Some(t), None)
            case _                 => (None, None)
          }
          val ts = sp.getOrElse(Nil).map(ctx.typeFor(_, Some(x)))
          val qs = proxyType.map(qualificationsFor(_, x)).map(_.mkString("::"))
          ctx << qs << "::" << proxyType.map(_.baseName) << sp.map(_ =>
            "<"
          ) << (ts, ",") << sp.map(_ => ">")
          ctx << "::" << member.map(ctx.nameFor(_)) << "(" << target
          ctx << Option.when(x.args.nonEmpty || implicits.nonEmpty)(",")
        } else proxy match {
          case Some(_) =>
            val targetType = target.map(ctx.exprType)
            val proxyType = targetType.flatMap(Find.tryClassLike).to[EirProxy]
            if (isLocal) {
              proxyType.foreach(ckLocalBranch(_, target.get))
              ctx << "->" << {
                if (member.exists(memberHasArgs)) {
                  member.map(
                    valueHandlerFor(ctx, _, GenerateProxies.valueSuffix)
                  )
                } else {
                  member.map(ctx.nameFor(_, Some(x)))
                }
              } << "("
            } else {
              ctx << (proxyType match {
                case Some(p) if p.isCollective || p.isSection =>
                  "ergoline::broadcast_value"
                case Some(_) => "hypercomm::interceptor::send_async"
                case _ =>
                  Errors.incorrectType(targetType.orNull, classOf[EirProxy])
              })
              ctx << "(" << target << ","
              ctx << member
                .zip(proxy)
                .flatMap({ case (m, _) =>
                  targetType.map(epIndexFor(_, m, target, Option(sp)))
                }) << ","
            }
          case None =>
            if (isPointer) ctx << "(*"
            ctx << x.target
            if (isPointer) ctx << ")"
        }

        if (!isProxy && proxy.forall(_ => functor)) {
          ctx << visitSpecialization(sp) << "("
        }

        ctx << visitArguments(
          Some(x),
          Some(disambiguated),
          (x.args, implicits)
        ) << ")"
      }
      if (isAsync) {
        ctx << "; return" << ctx.temporary << ";" << "})())"
      }
    }
  }

  def fieldAccessorFor(x: EirType)(static: Option[Boolean]): String = {
    if (static.contains(true)) {
      "::"
    } else if (x.isPointer(null)) {
      "->"
    } else {
      "."
    }
  }

  def iteratorNext(
      x: EirForAllHeader,
      iterName: String,
      fieldAccessor: String
  )(implicit ctx: CodeGenerationContext): Option[CppNode] = {
    x.declaration.map { decl =>
      val node = CppNode(s"$iterName${fieldAccessor}next()")
      node.foundType = Some(ctx.typeOf(decl))
      node
    }
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
        if (!declaration.forall(_.isInstanceOf[EirDeclaration])) {
          Errors.unsupportedOperation(
            x,
            "structured declarations in for-loops are unsupported",
            Errors.Limitation.CppCodeGen
          )
        }
        ctx << s"for (" <| (declaration, ";") << test << ";" << increment << ")" << "{" << x.body << "}"
      case h: EirForAllHeader =>
        val fieldAccessor = fieldAccessorFor(ctx.exprType(h.expression))(None)
        // TODO find a better name than it_
        val iterName = "it_"
        ctx << "{" << s"auto $iterName =" << h.expression << ";" << s"while ($iterName" << fieldAccessor << "hasNext()) {"
        h.declaration.foreach(
          visitDeclarationLike(_, iteratorNext(h, iterName, fieldAccessor))
        )
        // TODO add declarations
        ctx << x.body << "}" << "}"
      case _ => Errors.unreachable()
    }

    senti.foreach(popSentinel)
  }

  override def visitWhileLoop(
      x: EirWhileLoop
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << s"while (" << x.condition << ")" << "{" << x.body << "}"
  }

  override def visitLiteral(
      x: EirLiteral[_]
  )(implicit ctx: CodeGenerationContext): Unit = {
    x match {
      case EirStringLiteral(value) => ctx << s"std::string($value)"
      case EirLiteralSymbol(value) => ctx << ctx.nameFor(value, x.parent)
      case EirUnitLiteral(_)       =>
      case _                       => ctx << x.value.toString
    }
  }

  def asMember(x: EirNode): Option[EirMember] = {
    x match {
      case x: EirMember => Some(x)
      case _            => x.parent.to[EirMember]
    }
  }

  def asMember(x: Option[EirNode]): Option[EirMember] = x.flatMap(asMember)

  override def visitSymbol[A <: EirNamedNode](
      x: EirSymbol[A]
  )(implicit ctx: CodeGenerationContext): Unit = {
    val member = asMember(x.disambiguation)
    if (!CheckTypes.isSelf(x)) {
      member match {
        case Some(m: EirMember) if !m.isStatic =>
          ctx << {
            if (m.isEntry || m.isEntryOnly) {
              ctx.currentProxySelf
            } else {
              ctx.currentSelf
            }
          } << "->"
        case _ =>
      }
    }

    x.disambiguation match {
      case Some(c: EirClass) if c.objectType =>
        ctx << "ergoline::access_singleton<" << ctx.nameFor(c, Some(x)) << ">()"
      case _ => ctx << member
          .filter(_.isEntryOnly)
          .map(ctx.nameFor(_))
          .getOrElse(ctx.nameFor(x, Some(x)))
    }
  }

  override def visitDeclaration(
      x: EirDeclaration
  )(implicit ctx: CodeGenerationContext): Unit =
    visitDeclarationLike(x, x.initialValue)

  override def visitTemplateArgument(
      x: EirTemplateArgument
  )(implicit ctx: CodeGenerationContext): Unit = {
    val defaultValue = x.defaultValue.filter(_ =>
      x.parent.to[EirFunction].exists(_.functionArgs.isEmpty)
    )

    ctx << {
      x.argumentType
        .map(y => {
          if (x.parent.to[EirType].exists(isArray(ctx, _))) {
            "std::size_t" // TODO this one-off logic should be nixed!
          } else {
            ctx.typeFor(y, Some(x))
          }
        })
        .getOrElse("typename")
    } << (x match {
      case _ if x.isPack => s"... ${x.name}"
      case _             => x.name
    })

    defaultValue.foreach(ctx << "=" << _)
  }

  def visitInherits(
      x: EirClassLike
  )(implicit ctx: CodeGenerationContext): Unit = {
    val base = x.extendsThis.map(ctx.resolve)
    val serializableBase = base.map(ctx.typeOf).exists(!_.isTrait)
    val parents =
      (base ++ x.implementsThese.map(ctx.resolve)) map (ctx.nameFor(_))
    val declName = nameFor(
      ctx,
      x,
      includeTemplates = true
    )

    if (x.isInstanceOf[EirTrait]) {
      ctx << {
        if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(",")
        else ": public ergoline::trait<" + declName + ">"
      }
    } else {
      ctx << ": " << (parents.map("public " + _), ",")
      if (!serializableBase) {
        // TODO x.isTransient ?
        ctx << Option.when(parents.nonEmpty)(",") << {
          "public ergoline::object<" + declName + ">"
        }
      }
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
      ctx << "::CkAbort(\"cannot pup transient types\");"
    } else {
      val parents = puppingParents(ctx, x)
        .map(ctx.nameFor(_))
        .map(_ + s"::__pup__($puper);")
      val values = x.members
        .collect({
          case m @ EirMember(_, d: EirDeclaration, _)
              if m.annotation("transient").isEmpty && !m.isStatic => d
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
    val isDefined = x.templateArgs.isEmpty && ctx.hasChecked(x)
    val isSystem = x.isSystem

    if (isDefined || isSystem) {
      ctx << x.members.collect {
        case m @ EirMember(_, _: EirFunction, _) if !m.isSystem => m
      }

      if (!(isSystem || x.isTrait || GenerateDecls.hasPup(x))) {
        makePupper(ctx, x)
      }
    }
  }

  override def visitMember(
      x: EirMember
  )(implicit ctx: CodeGenerationContext): Unit = visit(x.member)

  def templateArgumentsToString(
      args: List[EirResolvable[EirType]],
      usage: Option[EirNode]
  )(ctx: CodeGenerationContext): String = {
    "<" + {
      args
        .map(ctx.resolve[EirResolvable[EirType]])
        .map(ctx.typeFor(_, usage))
        .mkString(",")
    } + ">"
  }

  def templateArgumentsToString(
      ctx: CodeGenerationContext,
      source: Option[EirResolvable[EirType]],
      params: List[EirResolvable[EirType]],
      usage: Option[EirNode] // = None
  ): String = {
    val templates = source
      .map(ctx.resolve(_))
      .flatMap(Find.tryClassLike)
      .map(templateArgsOf)

    val args = {
      if (templates.forall(_.length <= params.length)) {
        params
      } else {
        assert(templates.exists(_.length > params.length))
        params ++ templates
          .map(x => x.slice(params.length, x.length))
          .get
          .flatMap(_.defaultValue)
      }
    }

    templateArgumentsToString(args, usage)(ctx)
  }

  def templatize(x: EirType with EirSpecializable): EirType = {
    val args = templateArgsOf(x)
    if (args.nonEmpty) {
      EirTemplatedType(None, x, args.map(EirTemplateFacade))
    } else {
      x
    }
  }

  def templateArgsOf(
      x: EirSpecializable,
      systemParent: Boolean
  ): List[EirTemplateArgument] = {
    Option(x.templateArgs).filterNot(_.isEmpty) orElse {
      Option
        .when(systemParent)(x.dependentScope())
        .flatten
        .map(_.templateArgs)
    } getOrElse Nil
  }

  def templateArgsOf(x: EirSpecializable): List[EirTemplateArgument] = {
    templateArgsOf(x, x.systemParent)
  }

  def visitTemplateArgs(
      args: List[EirTemplateArgument]
  )(implicit ctx: CodeGenerationContext): Unit = {
    if (args.nonEmpty) ctx << s"template<" << (args, ",") << ">"
  }

  def visitTemplateArgs(x: EirSpecializable, systemParent: Boolean)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    visitTemplateArgs(templateArgsOf(x, systemParent))
  }

  def visitTemplateArgs(
      x: EirSpecializable
  )(implicit ctx: CodeGenerationContext): Unit = {
    visitTemplateArgs(templateArgsOf(x))
  }

  def generateAssignments(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx << x.functionArgs
      .filter(_.isSelfAssigning)
      .map(x => {
        "this->" + x.name + "=" + x.name + ";"
      })
  }

  def hasDependentScope(x: EirSpecializable): Boolean = {
    x.dependentScope().nonEmpty
  }

  def visitFunctionBody(
      x: EirFunction
  )(implicit ctx: CodeGenerationContext): Unit = {
    val member = x.parent.to[EirMember]
    val emptyBody = x.body.forall(_.children.isEmpty)
    var shouldEnclose = false

    if (member.exists(_.isConstructor)) {
      val parent = assertValid[EirClassLike](member.flatMap(_.parent))

      val currSelf = ctx.currentSelf
      val encapsulated = ctx.proxy.nonEmpty
      val assignments = x.functionArgs.filter(_.isSelfAssigning)
      val declarations = parent.members collect {
        case m @ EirMember(_, d: EirDeclaration, _)
            if !m.isStatic && d.initialValue.nonEmpty => d
      }

      shouldEnclose = assignments.nonEmpty || declarations.nonEmpty

      if (!encapsulated) {
        ctx << Option.when(shouldEnclose)(":")

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

      ctx << Option.when(shouldEnclose && !emptyBody)("{")

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
    ctx << Option.when(shouldEnclose && !emptyBody)("}")
  }

  def generateReturnType(x: EirFunction, isAsyncCi: Boolean)(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    if (isAsyncCi) {
      ctx << "void"
    } else {
      val retTy = ctx.typeFor(x.returnType)
      x.predicate match {
        case Some(p) =>
          ctx << "typename" << "std::enable_if<" << StaticGenerator.visit(
            p
          ) << "," << retTy << ">::type"
        case None => ctx << retTy
      }
    }
  }

  def pickSelf(
      fn: EirFunction,
      ancestor: Option[EirFunction]
  )(implicit
      ctx: CodeGenerationContext
  ): Option[String] = {
    val member = fn.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val systemParent = parent.exists(_.isSystem)
    val proxyParent = parent.to[EirProxy]
    val isStatic = member.exists(_.isStatic)
    if (ancestor.nonEmpty || isStatic) {
      None
    } else if (systemParent) {
      proxyParent.flatMap(_ => Errors.unreachable()).orElse(Some("self"))
    } else {
      proxyParent
        .map(_ => ctx.currentProxySelf + "->impl_")
        .orElse(Some("this"))
    }
  }

  private def systemNameFor(node: EirNode, where: Option[EirNode])(implicit
      ctx: CodeGenerationContext
  ): (Option[EirAnnotation], String) = {
    node match {
      case x: EirMember   => systemNameFor(x.member, where)
      case x: EirFunction => systemNameFor(x, where)
      case _              => ???
    }
  }

  private def systemNameFor(f: EirFunction, where: Option[EirNode])(implicit
      ctx: CodeGenerationContext
  ): (Option[EirAnnotation], String) = {
    val member = f.parent.to[EirMember]
    val system =
      f.annotation("system").orElse(member.flatMap(_.annotation("system")))
    val name =
      system.flatMap(_("alias")).map(_.strip()).getOrElse(ctx.nameFor(f, where))
    (system, name)
  }

  private def mkVirtualSystemCall(
      f: EirFunction
  )(implicit ctx: CodeGenerationContext): Unit = {
    val member = f.parent.to[EirMember]
    val isUnit = f.returnType match {
      case _: EirTemplatedType | _: EirSpecializedSymbol[_] => false
      case x                                                => ctx.typeOf(x) == globals.unitType
    }
    val (system, name) = systemNameFor(f, None)
    val isStatic = system.flatMap(_("static")).exists(_.toBoolean)
    val self = Option.when(isStatic)(
      if (member.exists(_.base.isValueType)) "*this"
      else "this->shared_from_this()"
    )

    ctx << Option.unless(isUnit)(
      "return"
    ) << name << "(" << (self ++ (f.functionArgs ++ f.implicitArgs).map(
      ctx.nameFor(_)
    ), ",") << ");"
  }

  def visitFunction(
      x: EirFunction,
      isMember: Boolean,
      entryKwd: Option[String] = None
  )(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    val member = x.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val entryOnly = member.exists(_.isEntryOnly) && ctx.proxy.isEmpty
    val abstractMember =
      !isMember && (parent.exists(_.isAbstract) && x.body.isEmpty)
    val langCi = ctx.language == "ci"
    val canEnter = langCi || ctx.hasChecked(x)

    val isSystem = x.isSystem
    val systemParent = parent.exists(_.isSystem)
    val virtualMember = member.exists(_.isVirtual)
    val avoidableSystem = isSystem && (systemParent || !virtualMember)
    val isGeneric = x.templateArgs.nonEmpty
    val definableTemplate =
      isGeneric && !isMember && !(parent.isEmpty || systemParent)

    if (
      !canEnter || (!langCi && entryOnly) || abstractMember || avoidableSystem || definableTemplate
    ) {
      return
    }

    val ancestor = Find.ancestors(x).collectFirst { case fn: EirFunction => fn }
    val isNested = ancestor.nonEmpty
    val isConstructor = member.exists(_.isConstructor)
    val isStatic = member.exists(_.isStatic)
    val proxyParent = parent.to[EirProxy]

    if (isNested && isGeneric) {
      Errors.unsupportedOperation(
        x,
        "generic nested functions",
        Limitation.CppCodeGen
      )
    }

    val annotatable = isMember && !(systemParent || isNested || langCi)
    val asyncCi =
      langCi && isMember && member.flatMap(_.annotation("async")).isDefined
    val overrides =
      Option.when(annotatable && member.exists(_.isOverride))(" override")
    val virtual = Option.when(annotatable && virtualMember)("virtual")

    // TODO this is a temporary solution that may cause failures when
    //      function bodies are defined ahead of their used symbols
    val nestedParent = parent.flatMap(_.parent).to[EirMember].nonEmpty
    val pureVirtual = virtual.nonEmpty && x.body.isEmpty
    val forwardDeclaration = langCi || systemParent || (
      x.templateArgs.isEmpty && !nestedParent && !hasDependentScope(x)
    )

    val willReturn = isMember && (pureVirtual || forwardDeclaration)
    if (!willReturn && x.hasAnnotation("stencil")) {
      GenerateStencil.visit(x)
    }

    visitTemplateArgs(x, systemParent)
    ctx << entryKwd
    ctx << virtual
    // TODO add templates when !isMember
    if (!(isConstructor || isNested)) generateReturnType(x, asyncCi)
    val args = x.functionArgs ++ x.implicitArgs

    parent match {
      case _ if isNested                                => ctx << "auto" << ctx.nameFor(x) << "=" << "[&]"
      case Some(p: EirProxy) if langCi && isConstructor => ctx << p.baseName
      case Some(classLike) if isConstructor =>
        ctx << Option.unless(isMember)(
          ctx.nameFor(classLike) + "::"
        ) << declNameFor(classLike)
      case Some(classLike) => ctx << Option.unless(isMember || systemParent)(
          ctx.nameFor(classLike) + "::"
        ) << ctx.nameFor(x)
      case _ => ctx << ctx.nameFor(x)
    }

    val currSelf = pickSelf(x, ancestor)
    currSelf.foreach(ctx.pushSelf)

    val addSelf = systemParent && !isNested
    val pointerParent = parent.exists(_.isPointer)
    val parentType = parent
      .filter(_ => addSelf)
      .map(p => {
        val name = qualifiedNameFor(ctx, x, includeTemplates = true)(p)

        if (p.isPointer) {
          s"std::shared_ptr<$name>"
        } else {
          name
        }
      })

    ctx << "("

    parentType.zip(currSelf).foreach { case (ty, self) =>
      ctx << "const" << ty << "&" << {
        if (pointerParent) self else s"${self}_"
      } << {
        Option.when(args.nonEmpty)(",")
      }
    }

    if (proxyParent.isDefined && (args.nonEmpty || asyncCi)) {
      ctx << s"CkMessage* ${GenerateProxies.msgName}"
    } else {
      ctx << (args, ",")
    }
    ctx << ")" << overrides

    if (isMember) {
      if (isSystem && virtualMember) {
        ctx << "{" << mkVirtualSystemCall(x) << "}"
        return
      }
      if (pureVirtual) {
        ctx << " = 0;"
        return
      } else if (forwardDeclaration) {
        ctx << ";"
        return
      }
    } else if (x.body.isEmpty) {
      Errors.missingBody(x)
    }

    assert(isStatic || isNested || currSelf.nonEmpty)

    parentType.zip(currSelf).filterNot(_ => pointerParent).foreach {
      case (ty, self) =>
        ctx << "{"
        ctx << "auto*" << self << "=" << s"const_cast<$ty*>(&${self}_);"
        ctx.ignoreNext("{")
    }

    visitFunctionBody(x)
    if (isNested) {
      ctx.appendLast(";")
    }

    currSelf.foreach(_ => ctx.popSelf())
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
      case None    => ctx << "(" << x.op << "(" << x.rhs << "))"
    }
  }

  override def visitBinaryExpression(
      x: EirBinaryExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    x.disambiguation match {
      case Some(x) => ctx << x
      case None => ctx << "(" << x.lhs << {
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

    val sys = base.annotation("system")
    if (
      node.isInstanceOf[EirTemplateArgument] || (
        sys.isDefined && !sys.flatMap(_("qualify")).exists(_.toBoolean)
      )
    ) {
      return Nil
    }

    val ours = Find.parentOf[EirNamespace](within)
    val theirs = Find.parentOf[EirNamespace](base)

    theirs match {
      case Some(theirs) if !ours.contains(theirs) =>
        (theirs.name +: Find.ancestors(theirs).collect { case n: EirNamespace =>
          n.name
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
    of match {
      case x: EirConstantFacade => expressionToString(x.value)(ctx)
      case x => (qualificationsFor(x, occurrence)(ctx) :+ nameFor(
          ctx,
          x,
          includeTemplates,
          Some(occurrence)
        )).mkString("::")
    }
  }

  def selfIndex(ctx: CodeGenerationContext): String =
    s"${ctx.currentProxySelf}->__index__()"

  def selfName(ctx: CodeGenerationContext, s: EirSymbol[_]): String = {
    s.qualifiedName.last match {
      case "self@" =>
        val proxySelf = ctx.currentProxySelf
        if (proxySelf == GenerateProxies.proxyMemberProxySelf) {
          ctx.currentSelf
        } else {
          ctx.proxy
            .flatMap(_.collective)
            .map(_ => s"${ctx.currentProxySelf}->thisProxy")
            .getOrElse(
              s"${ctx.currentProxySelf}->thisProxy[${ctx.currentProxySelf}->thisIndexMax]"
            )
        }
      case "self[@]" => s"${ctx.currentProxySelf}->thisProxy[" + {
          s"hypercomm::conv2idx<CkArrayIndex>(${selfIndex(ctx)})"
        } + "]"
      case _ => selfName(ctx, s.asInstanceOf[EirNode])
    }
  }

  def selfName(ctx: CodeGenerationContext, n: EirNode): String = {
    val ty = CheckTypes.stripReference(n match {
      case e: EirExpressionNode               => ctx.exprType(e)
      case d: EirDeclaration                  => ctx.resolve(d.declaredType)
      case EirMember(_, d: EirDeclaration, _) => ctx.resolve(d.declaredType)
      case _                                  => Errors.missingType(n)
    })

    val currSelf = ctx.currentSelf
    if (ty.isPointer(ctx)) {
      if (currSelf != "this") {
        currSelf
      } else if (ty.isTrait) {
        "this->__self__()"
      } else {
        // TODO this should only be shared_from_this when casting!
        "(" + nameFor(
          ctx,
          ty,
          includeTemplates = true
        ) + "::shared_from_this())"
      }
    } else {
      s"(*$currSelf)"
    }
  }

  private def nameForProxyMember(p: Option[EirProxy], x: EirMember)(implicit
      ctx: CodeGenerationContext
  ): String = {
    Option
      .when(x.isEntry || x.isEntryOnly)({
        assert(p.orElse(ctx.proxy).isDefined)
        x.counterpart.getOrElse(x).ordinal
      })
      .flatten match {
      case Some(ord) => s"__${x.name}_${ord}__"
      case _         => x.name
    }
  }

  def declNameFor(
      x: EirNode
  )(implicit
      ctx: CodeGenerationContext
  ): Option[String] = {
    val staticFn = asMember(x) exists {
      case m @ EirMember(_, _: EirFunction, _) => m.isStatic
      case _                                   => false
    }
    val system = x.annotation("system")
    val prefix = x.memberParent
      .filter(y => system.isEmpty && y.isSystem)
      .collect { case n: EirNamedNode => n.name }
      .getOrElse("") + (if (staticFn && system.isEmpty) "__s" else "")
    val alias = system.flatMap(_("alias")).map(_.strip())
    val isOperator = system.flatMap(_("operator")).exists(_.toBoolean)
    alias
      .orElse(x match {
        case n: EirNamedNode =>
          if (isOperator || n.name.forall(x => x.isLetterOrDigit || x == '_'))
            Some(n.name)
          else Some(globals.encodeOperator(n.name))
        case _ => None
      })
      .map(x => {
        if (x == "std::size_t" && ctx.language == "ci") "size_t"
        else x
      })
      .map(b => {
        if (prefix.nonEmpty) s"${prefix}_$b" else b
      })
  }

  private def nameFor(
      s: EirSpecializable,
      name: String,
      includeTemplates: Boolean,
      usage: Option[EirNode]
  )(implicit
      ctx: CodeGenerationContext
  ): String = {
    val isClass = s.isInstanceOf[EirClassLike]
    val systemParent = s.systemParent
    val templateArgs = templateArgsOf(s, systemParent)
    if (templateArgs.nonEmpty) {
      val subst = templateArgs.map(ctx.hasSubstitution)
      val substDefined = subst.forall(_.isDefined)
      val shouldGen: Boolean = {
        // TODO this inherently reflects the assumption that
        //      nested classes are private!
        includeTemplates || substDefined || (systemParent && isClass)
      }
      name + Option
        .when(shouldGen)({
          "<" + ({
            if (substDefined) subst.flatten.map(ctx.typeFor(_, usage))
            else templateArgs.map(ctx.nameFor(_))
          } mkString ",") + ">"
        })
        .getOrElse("")
    } else {
      name
    }
  }

  def expressionToString(
      x: EirExpressionNode
  )(implicit ctx: CodeGenerationContext): String = {
    val child = ctx.makeSubContext()
    child << x
    child.toString
  }

  def nameFor(
      ctx: CodeGenerationContext,
      x: EirNode,
      includeTemplates: Boolean = false,
      usage: Option[EirNode] = None
  ): String = {
    val dealiased = declNameFor(x)(ctx)
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
                ) => s"(${ctx.currentProxySelf}->__element__())"
            case _ => Errors.unableToResolve(s)
          }
        }
      case m: EirMember if m.isEntryOnly || proxy.isDefined =>
        nameForProxyMember(
          {
            proxy.orElse(m.counterpart.flatMap(_.parent).to[EirProxy])
          },
          m
        )(ctx)
      case _: EirFunction if proxy.isDefined =>
        nameForProxyMember(proxy, asMember(opt).get)(ctx)
      case _ if proxy.isDefined =>
        val prefix =
          if (proxy.exists(_.singleton)) "CProxyElement_" else "CProxy_"
        val name = prefix + proxy.get.baseName
        x match {
          case t: EirTemplatedType =>
            name + templateArgumentsToString(ctx, Some(t.base), t.args, usage)
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
        val base = ctx.resolve(x.base)
        nameFor(ctx, base, usage = usage) + {
          Option
            .unless(isFuture(base))({
              templateArgumentsToString(ctx, Some(base), x.args, usage)
            })
            .getOrElse("")
        }
      case x: EirTypeAlias =>
        if (x.templateArgs.isEmpty) {
          expressionToString(x.value)(ctx)
        } else {
          ???
        }
      case x: EirSpecializable with EirNamedNode =>
        nameFor(x, dealiased.get, includeTemplates, usage)(ctx)
      case _: EirNamedNode => dealiased.get
      case t: EirTupleType =>
        s"std::tuple${templateArgumentsToString(ctx, None, t.children, usage)}"
      case s: EirConstantFacade => s.value.toString
      case x: EirLambdaExpression => _lambda_names.get(x) match {
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
      case b: EirTemplateFacade => nameFor(ctx, b.t, includeTemplates, usage)
      case t: EirReferenceType =>
        nameFor(ctx, t.base, includeTemplates, usage) + "&"
      case x: EirTupleMultiply =>
        val lhs = nameFor(ctx, x.lhs, includeTemplates, usage)
        val rhs = expressionToString(x.rhs)(ctx)
        s"ergoline::tuple_multiply_t<$lhs, $rhs>"
    }
    if (ctx.hasPointerOverride(x)) s"(*$result)" else result
  }

  def typeForEntryArgument(
      ctx: (CodeGenerationContext, EirNode)
  )(ty: EirResolvable[EirType]): String = {
    val resolution = ctx._1.resolve[EirNode](ty)
    resolution match {
      case _: EirTemplateArgument => ctx._1.nameFor(resolution, Some(ctx._2))
      case t: EirTupleType => "std::tuple<" + {
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
    val fn = Find.ancestors(x) collectFirst { case x: EirAssignment =>
      x
    } match {
      case Some(a) if onLeftSide(a, x) => "forward_as_tuple"
      case _                           => "make_tuple"
    }

    ctx << s"std::$fn(" << (x.children, ",") << ")"
  }

  private val _lambda_names: mutable.Map[EirLambdaExpression, String] =
    mutable.Map()

  override def visitLambdaExpression(
      x: EirLambdaExpression
  )(implicit ctx: CodeGenerationContext): Unit = {
    val ty = ctx.typeOf(x)
    val wasCaptured: EirNode => Boolean = {
      case d: EirDeclaration => d.captured
      case _                 => false
    }
    val captures = x.captures.map(captured => {
      // TODO use specialized version when avail
      val ty = CheckTypes.stripReference(ctx.typeOf(captured))
      val name = ctx.nameFor(captured, Some(x))
      if (ty.isPointer) name
      else if (wasCaptured(captured)) {
        captured.name
      } else {
        val t = ctx.typeFor(ty, Some(x))
        s"std::shared_ptr<$t>(std::shared_ptr<$t>{}, &$name)"
      }
    })
    ctx << {
      "std::static_pointer_cast<" + ctx.nameFor(ty, Some(x)) + ">"
    } << "(" << "std::make_shared<" << ctx.nameFor(
      x
    ) << ">(" << (captures, ",") << ")" << ")"
  }

  def makeLambdaWrapper(implicit
      ctx: CodeGenerationContext,
      lambda: EirLambdaExpression
  ): Unit = {
    val name = ctx.nameFor(lambda)
    val captures = lambda.captures
    val ty = assertValid[EirLambdaType](ctx.exprType(lambda))
    assert(ty.templateArgs.isEmpty)
    val args = (ty.to +: ty.from).map(ctx.typeFor(_))
    val ctypes = captures.map(ctx.typeOf(_)).map(CheckTypes.stripReference)
    val isTransient = ctypes.exists(_.isTransient)
    val cdecltypes = ctypes.map(_t => {
      val t = ctx.typeFor(_t, Some(lambda))
      if (_t.isPointer) t else s"std::shared_ptr<$t>"
    })
    ctx << s"struct $name: public ergoline::function<${args mkString ","}> {"
    ctx << s"$name(${cdecltypes.zipWithIndex
      .map({ case (t, idx) => s"$t _$idx" }) mkString ","})" << Option.when(
      captures.nonEmpty
    )(": ") << {
      (
        captures.zipWithIndex.map({ case (n, idx) => s"${n.name}(_$idx)" }),
        ","
      )
    } << "{ }"
    //    if () ctx << s"$name() { }"
    ctx << s"$name(PUP::reconstruct __tag__): ergoline::function<${args mkString ","}>(__tag__) { }"
    ctx << "virtual void __pup__(hypercomm::serdes& _) override" << "{"
    if (isTransient) {
      ctx << "::CkAbort(\"lambda" << name << "is transient and cannot be pup'd.\""
    } else {
      ctx << captures
        .zip(ctypes)
        .flatMap({ case (n, t) => pupperFor((ctx, lambda, "_"))(n.name, t) })
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
      .map({ case (n, t) => s"$t ${n.name};" })
    ctx << "};"
  }

  @tailrec
  def isArray(ctx: CodeGenerationContext, t: EirType): Boolean = {
    t match {
      case t: EirReferenceType => isArray(ctx, ctx.resolve(t.base))
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
    val rsv = CheckTypes.stripReference(ctx.resolve(t))
    rsv match {
      case t: EirTupleType => t.children.exists(containsArray(ctx, _))
      case t               => isArray(ctx, t)
    }
  }

  def arrayDim(
      ctx: CodeGenerationContext,
      t: EirType
  ): Option[EirLiteral[_]] = {
    val rsv = CheckTypes.stripReference(ctx.resolve(t))
    rsv match {
      case t: EirTemplatedType if isArray(ctx, t) =>
        t.args match {
          case _ +: Nil      => Some(EirIntegerLiteral(1)(None))
          case _ +: t +: Nil => Some(ctx.eval2const(t))
          case _             => None
        }
      case _ => None
    }
  }

  def createArray(
      name: String,
      args: List[EirExpressionNode],
      n: Int,
      initialize: Boolean
  )(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    ctx << name << "::instantiate(std::array<std::size_t," << n.toString << ">"
    ctx << "{ (std::size_t) " << (args, ", (std::size_t) ") << s" },${initialize.toString.toLowerCase})"
  }

  def createArray(
      name: String,
      args: List[EirExpressionNode],
      n: EirLiteral[_],
      initialize: Boolean
  )(implicit
      ctx: CodeGenerationContext
  ): Unit = {
    n match {
      case EirIntegerLiteral(x) => createArray(name, args, x, initialize)
      case _                    => ???
    }
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
        val numTake = collective match {
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
            Option
              .when(disambiguate(ctx, x) match {
                case x: EirMember => x.isSystem
                case _            => false
              })(-1)
              .getOrElse(0)
          case _ => ???
        }
        val postDrop =
          if (numTake < 0) args.dropRight(numTake.abs) else args.drop(numTake)
        if (postDrop.nonEmpty) {
          ctx << visitArguments(None, x.disambiguation, postDrop) << Option
            .unless(numTake == 0)(",")
        }
        if (numTake < 0) {
          ctx << (args.slice(args.length + numTake, args.length), ",")
        } else if (numTake > 0) {
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
        val name = ctx.nameFor(t, Some(x))
        arrayDim(ctx, t) match {
          case Some(n) => createArray(name, args, n, initialize = true)
          case None =>
            ctx << "std::make_shared<ergoline::extricate_t<" << name << ">>(" << visitArguments(
              None,
              x.disambiguation,
              args
            ) << ")"
        }
      case _ => ctx << Option.when(objTy.isPointer)("new") << ctx.typeFor(
          objTy,
          Some(x)
        ) << "(" << visitArguments(None, x.disambiguation, args) << ")"
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
      "::CkAbort(\"no match found at " + location.substring(
        location.lastIndexOf(File.separator) + 1
      ) + "\");"
    } << "})(" << x.expression << ")"
  }

  private def pickName(current: String, ctx: CodeGenerationContext)(
      x: EirExtractorPattern
  ): String = {
    val patterns = x.list.map(_.patterns).getOrElse(Nil)
    patterns match {
      case EirIdentifierPattern(_, name, _) :: Nil => name
      case _                                       => current + ctx.temporary
    }
  }

  type LazyCodeBlock = () => CodeGenerationContext
  type DeclarationFormatter =
    (CodeGenerationContext, String, Option[String], LazyCodeBlock) => Unit

  def defaultFormat(
      ctx: CodeGenerationContext,
      name: String,
      ty: Option[String],
      blk: LazyCodeBlock
  ): Unit = {
    // TODO ( eventually, we need to use ty.getOrElse("auto") here )
    // TODO ( but typing is quite weak here -- needs some buffing up )
    ctx << "auto" << name << "=" << blk() << ";"
  }

  def visitPatternDecl(
      parent: CodeGenerationContext,
      x: EirPattern,
      current: String,
      forceTuple: Boolean = false,
      format: DeclarationFormatter = defaultFormat
  ): String = {
    implicit val ctx = parent.makeSubContext()
    x match {
      case x: EirExtractorPattern => x.disambiguation.foreach(f => {
          val tmp = pickName(current, parent)(x)
          val node = CppNode(current)
          node.foundType = f.args.lastOption.flatMap(_.foundType)
          val ty = node.foundType.map(ctx.typeFor(_, Some(x)))
          f.args = List(EirCallArgument(node, isRef = false)(Some(f)))
          format(ctx, tmp, ty, () => { ctx << f })
          x.list.map(_.patterns).filterNot(_.length <= 1) match {
            case Some(patterns) =>
              patterns.zipWithIndex.foreach { case (p, i) =>
                val nextTmp = s"((bool)$tmp ? &std::get<$i>(*$tmp) : nullptr)"
                ctx << visitPatternDecl(ctx, p, nextTmp, format = format)
              }
            case None =>
          }
        })
      case EirPatternList(_, ps) => ps match {
          case p :: Nil if !forceTuple =>
            ctx << visitPatternDecl(ctx, p, current, format = format)
          case p :: Nil => ctx << visitPatternDecl(
              ctx,
              p,
              s"std::get<0>($current)",
              format = format
            )
          case patterns => ctx << patterns.zipWithIndex.map({ case (p, idx) =>
              visitPatternDecl(
                ctx,
                p,
                s"std::get<$idx>($current)",
                format = format
              )
            })
        }
      case i @ EirIdentifierPattern(_, n, t) if n != "_" =>
        val declName = i.declarations.headOption.map(ctx.nameFor(_))
        val ty = ctx.resolve(t)
        val fmtTy = Some(ctx.typeFor(ty, Some(i)))
        if (ty.isPointer && i.needsCasting) {
          declName.foreach(
            format(
              ctx,
              _,
              fmtTy,
              () => {
                ctx << "std::dynamic_pointer_cast<" << ctx.nameFor(
                  t
                ) << ">(" << current << ")"
              }
            )
          )
        } else {
          // TODO make this a reference!
          declName.foreach(format(ctx, _, fmtTy, () => { ctx << current }))
        }
      case i: EirIdentifierPattern => if (i.name != "_") Errors.missingType(x)
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
      case x: EirExtractorPattern =>
        val nextTemp = pickName(current, parent)(x)
        val ty = x.disambiguation.flatMap(_.foundType)
        List("(bool)" + nextTemp) ++ x.list.toList.flatMap(list =>
          visitPatternCond(
            parent,
            list,
            nextTemp,
            ty
          )
        )
      case EirPatternList(_, ps) => ps match {
          case p :: Nil => parentType match {
              case Some(t: EirTupleType) => visitPatternCond(
                  parent,
                  p,
                  s"std::get<0>($current)",
                  t.children.headOption.map(parent.resolve[EirType])
                )
              case _ => visitPatternCond(parent, p, current, parentType)
            }
          case patterns => patterns.zipWithIndex.flatMap { case (p, idx) =>
              visitPatternCond(
                parent,
                p,
                s"std::get<$idx>($current)",
                typeAt(parent, parentType, idx)
              )
            }
        }
      case i @ EirIdentifierPattern(_, n, t)
          if parent.resolve(t).isPointer(parent) =>
        val wildcard = n == "_"
        parentType match {
          case None => Errors.missingType(x)
          // TODO use a more reliable comparison here!
          case Some(_) if !i.needsCasting => Nil
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

  def findPointers(
      x: EirPattern
  )(implicit ctx: CodeGenerationContext): List[EirNode] = {
    x match {
      case x: EirPatternList => x.patterns.flatMap(findPointers)
      case x: EirExtractorPattern => x.list.toList
          .flatMap(_.patterns)
          .flatMap(_.declarations)
          .filterNot(ctx.typeOf(_).isPointer)
      case _ => Nil
    }
  }

  override def visitMatchCase(
      x: EirMatchCase
  )(implicit ctx: CodeGenerationContext): Unit = {
    val parent = x.parent.to[EirMatch]
    val exprType = parent.map(_.expression).map(ctx.exprType)
    val hasBlock = x.body.to[EirBlock].nonEmpty
    val isUnit = parent
      .map(ctx.exprType)
      .contains(globals.unitType)
    ctx << "{" << visitPatternDecl(ctx, x.patterns, ctx.temporary).split(n)
    val conditions = visitPatternCond(ctx, x.patterns, ctx.temporary, exprType)
      .mkString(" && ")
    val ptrs = findPointers(x.patterns)
    val needsIf = x.condition.nonEmpty || conditions.nonEmpty
    if (needsIf) ctx << "if(" << x.condition << {
      Option.when(x.condition.isDefined && conditions.nonEmpty)(" && ")
    } << conditions << ")" << "{"
    val (primary, secondary) = (
      Option.unless(isUnit || hasBlock)("return"),
      Option.when(isUnit)("return;")
    )
    ptrs.foreach(ctx.makePointer)
    ctx << primary << visitOptionalStatement(x.body) << secondary << Option
      .when(needsIf)(
        "}"
      ) << "}"
    ptrs.foreach(ctx.unsetPointer)
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

  def makeArrayIndex(
      is: List[EirExpressionNode]
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "hypercomm::conv2idx<CkArrayIndex>(" << {
      if (is.length == 1) {
        ctx << is.head
      } else {
        ctx << "std::make_tuple(" << (is, ",") << ")"
      }
    } << ")"
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
        val arg = arrayRef.args.headOption.map(ctx.eval2const(_))
        arg match {
          case Some(EirIntegerLiteral(x)) => ctx << {
              // TODO eliminate this once (array::shape) is fixed?
              val thisTy = ctx.resolve(tty.children(x))
              Option.unless(thisTy.isPointer)(
                "(" + ctx.typeFor(thisTy, Some(arrayRef)) + ")"
              )
            } << s"std::get<" << x.toString << ">(" << arrayRef.target << ")"
          case _ => Errors.invalidTupleIndices(tty, arrayRef.args)
        }
      case _ if collective.isDefined =>
        ctx << arrayRef.target << "[" << makeArrayIndex(arrayRef.args) << "]"
      case t if isArray(ctx, t) =>
        val target = arrayRef.target
        val args = arrayRef.args
        if (args.exists(_.isInstanceOf[EirSlice])) {
          val dis = arrayRef.disambiguation match {
            case Some(x: EirFunctionCall) => x.target.disambiguation
            case _                        => ???
          }
          val name = dis.map(systemNameFor(_, Some(arrayRef))(ctx)).map(_._2)
          ctx << name << "(" << target << ",std::forward_as_tuple(" << (args, ",") << "))"
        } else {
          ctx << "(*" << target << ")["
          args.init.zipWithIndex.foreach { case (arg, idx) =>
            ctx << arg << "*" << s"(" << target << s"->shape[${args.length - (idx + 1)}])" << "+"
          }
          ctx << args.last
          ctx << "]"
        }
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

  override def visitSpecializedSymbol[A <: EirNamedNode](
      x: EirSpecializedSymbol[A]
  )(implicit ctx: CodeGenerationContext): Unit = {
    val base = ctx.resolve(x.symbol)
    ctx << ctx.nameFor(base) << visitSpecialization(x)
  }

  override def visitIfElse(
      x: EirIfElse
  )(implicit ctx: CodeGenerationContext): Unit = {
    ctx << "if" << "(" << x.test << ")" << visitOptionalStatement(
      x.ifTrue
    ) << x.ifFalse.map(_ => "else") << visitOptionalStatement(
      x.ifFalse,
      alt = ""
    )
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
    val visitChild: EirNode => Unit = {
      case x: EirExpressionNode =>
        ctx << x << Option.unless(x.isInstanceOf[CppNode])(";")
      case x => ctx << x
    }

    val matchCase = x.parent.to[EirMatchCase]
    if (matchCase.nonEmpty) {
      val foundType = matchCase.flatMap(_.parent).to[EirMatch].map(ctx.exprType)
      x.children.init.foreach(visitChild)
      x.children.lastOption.foreach {
        case x: EirExpressionNode => ctx << Option.unless(
            foundType.contains(globals.unitType)
          )("return") << x << ";"
        case x => ctx << x
      }
    } else {
      val topLevel = x.parent exists {
        case _: EirLambdaExpression => true
        case _: EirFunction         => true
        case _                      => false
      }

      val singleStatement = x.children.length == 1 && !x.children.exists(
        _.isInstanceOf[EirDeclaration]
      )

      ctx << Option.unless(!topLevel && singleStatement)("{")
      x.children.foreach(visitChild)
      ctx << Option.unless(!topLevel && singleStatement)("}")
    }
  }

  def visitStatement(
      node: EirNode
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    node match {
      case x: EirExpressionNode =>
        ctx << x << Option.unless(x.isInstanceOf[CppNode])(";")
      case _ => ctx << node
    }
  }

  def visitOptionalStatement(
      opt: Option[EirNode],
      alt: String = ";"
  )(implicit ctx: CodeGenerationContext): CodeGenerationContext = {
    opt match {
      case Some(x) => visitStatement(x)
      case None    => ctx << alt
    }
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

  def isPlainArrayRef(x: EirArrayReference): Boolean = {
    val target = x.disambiguation
      .to[EirFunctionCall]
      .flatMap(_.target.disambiguation)
    val system = target.flatMap(_.annotation("system"))
    system
      .flatMap(a =>
        a("alias")
          .map(_.strip())
          .orElse(target collect { case n: EirNamedNode => n.name })
      )
      .contains("[]")
  }

  def implicitCast(
      pair: (EirExpressionNode, EirType),
      goal: EirType,
      requiresRef: Boolean
  )(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val (value, valueTy) = pair

    (
      Find.tryClassLike(goal).getOrElse(goal),
      Find.tryClassLike(valueTy)
    ) match {
      case (a: EirProxy, Some(b: EirProxy)) if b.isDescendantOf(a) =>
        ctx << ctx.nameFor(a) << "(" << value << ")"
      case (a: EirClassLike, Some(b)) if a.isPointer && b.isValueType =>
        structToTrait(value, valueTy, noCopy = requiresRef)
      case (a: EirLambdaType, Some(b)) =>
        ctx << value.disambiguation.to[EirLambdaExpression]
      case _ => ctx << value
    }
  }

  def assignmentRhs(lhsTy: EirType, op: String, rhs: EirExpressionNode)(implicit
      ctx: CodeGenerationContext
  ): CodeGenerationContext = {
    val rhsPair = (rhs, ctx.typeOf(rhs))
    ctx << op << implicitCast(rhsPair, lhsTy, requiresRef = false)
  }

  override def visitAssignment(
      assign: EirAssignment
  )(implicit ctx: CodeGenerationContext): Unit = {
    assign.disambiguation match {
      case Some(x) => ctx << x
      case None =>
        val lhsTy = ctx.resolve(ctx.typeOf(assign.lval))
        assign.lval match {
          case x: EirArrayReference if !isPlainArrayRef(x) => ctx << x
          case x                                           => ctx << x << assignmentRhs(lhsTy, assign.op, assign.rval)
        }
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
        )(None) << "get()" << ";"
        ctx << tmp << fieldAccessorFor(found)(None) << "release()" << ";"
        ctx << "return" << "val" << ";"
        ctx << "}" << ")(" << target << ")"
      case None => ctx << x.disambiguation
    }
  }

  // TODO implement this?
  private def escapeInterpString(s: String) = "std::string(\"" + s + "\")"

  override def visitInterpolatedString(
      str: EirInterpolatedString
  )(implicit ctx: CodeGenerationContext): Unit = {
    def matchChild(x: EirExpressionNode): CodeGenerationContext = {
      x match {
        case EirStringLiteral(value) if !value.startsWith("\"") =>
          ctx << escapeInterpString(value)
        case _ => ctx << "(" << x << ")"
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
  )(implicit context: CodeGenerationContext): Unit = visit(facade.value)

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
      body: EirNode
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
            .collect { case i: EirIdentifierPattern =>
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

  type RequestList = List[(EirMember, String, EirType, List[Int])]

  def generateRequestList(
      when: EirSdagWhen
  )(implicit ctx: CodeGenerationContext): RequestList = {
    def encapsulate(types: List[EirResolvable[EirType]]): EirType = {
      types.toTupleType(allowUnit = true)(None).asInstanceOf[EirType]
    }

    when.patterns.map({ case (symbol, _) =>
      val accessors = Find.resolveAccessor(EirScopedSymbol(null, symbol)(None))(
        ctx.proxy,
        Some(false)
      )(ctx.tyCtx)
      val m =
        accessors.headOption.map(_._1).getOrElse(Errors.unableToResolve(symbol))
      val f = assertValid[EirFunction](m.member)
      val declTys = f.functionArgs.map(_.declaredType).map(ctx.resolve)
      val tys = declTys.map(ctx.typeFor(_, Some(when)))
      val resolved = ctx.resolve(encapsulate(declTys))
      val mboxName = GenerateProxies.mailboxName(ctx, f, tys)
      val arrayArgs = f.functionArgs.zipWithIndex
        .filter(x => isArray(ctx, ctx.typeOf(x._1)))
        .map(_._2)
      (m, mboxName, resolved, arrayArgs)
    })
  }

  def generateRequest(
      when: EirSdagWhen,
      list: RequestList,
      com: String
  )(implicit ctx: CodeGenerationContext): Unit = {
    when.patterns.zipWithIndex.foreach({ case ((_, patterns), i) =>
      val (_, mboxName, resolved, _) = list(i)
      val temp = ctx.temporary
      val tempVal = s"$temp->value()"
      val declarations = visitPatternDecl(ctx, patterns, tempVal).split(n)
      val conditions = visitPatternCond(ctx, patterns, tempVal, Some(resolved))
        .mkString(" && ")

      val pred: Option[String] = Option.when(conditions.nonEmpty)({
        val name = s"__pred${i}__"
        val ty = name.init + "arg_type__"
        val constRef = s"const $ty&"
        ctx << "{"
        ctx << "using" << ty << "=" << "typename" << s"${GenerateProxies.mailboxType(mboxName)}::type;"
        ctx << s"auto" << name << s"=ergoline::wrap_lambda<bool, $constRef>([=]($constRef $temp)" << "->" << "bool" << "{"
        ctx << declarations
        ctx << "return" << conditions
        ctx << when.condition.map(_ => "&&") << when.condition
        ctx << ";" << "});"
        name
      })

      ctx << ctx.currentProxySelf << "->" << s"$mboxName->put_request_to(" << pred
        .getOrElse("{},") << pred
        .map(_ => ",") << com << "," << i.toString << ");"

      pred.foreach(_ => ctx << "}")
    })
  }

  def joinRequestTypes(list: RequestList): String = {
    s"std::tuple<${list
      .map { case (_, mboxName, _, _) =>
        s"typename ${GenerateProxies.mailboxType(mboxName)}::type"
      }
      .mkString(", ")}>"
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

    val nPorts = x.patterns.length
    val com = sentinel._3
      .map(s => {
        val n = s"__com${s.length}__"
        s.push(n)
        n
      })
      .getOrElse("__com__")

    if (sentinel._3.isEmpty) ctx << "{"

    val quadruplets = generateRequestList(x)

    val set = s"${com}vals__"
    val setType = s"${set}type__"

    ctx << "using" << setType << "=" << joinRequestTypes(quadruplets) << ";"

    ctx << "auto" << com << "=" << "ergoline::make_component<" << setType << ">(*" << ctx.currentProxySelf << "," << nPorts.toString << ","
    ctx << "[=](" << setType << "&" << set << ")" << "{"

    x.patterns.zipWithIndex.foreach({ case ((_, patterns), i) =>
      val (m, _, _, arrayArgs) = quadruplets(i)
      val name = s"__value${i}__"

      ctx << "auto" << name << "=" << s"std::move(std::get<$i>($set));"

      ctx << visitPatternDecl(
        ctx,
        patterns,
        name + "->value()"
      ).split(n)

      if (arrayArgs.nonEmpty) {
        x.body.map(findInplaceOpportunities(m, arrayArgs, _))
      }
    })

    ctx << visitOptionalStatement(x.body) << "}" << ");"

    generateRequest(x, quadruplets, com)

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
      ctx << ctx.currentProxySelf << "->activate_component(" << com << ");"
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

        s.foreach(com =>
          ctx << ctx.currentProxySelf << "->activate_component(" << com << ");"
        )
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

  override def visitReferenceType(x: EirReferenceType)(implicit
      ctx: CodeGenerationContext
  ): Unit = ???

  def visitDeclarationLike(node: EirNode, value: Option[EirExpressionNode])(
      implicit ctx: CodeGenerationContext
  ): Unit = {
    node match {
      case x: EirDeclaration =>
        val ty = ctx.resolve(x.declaredType)
        if (x.captured && !ty.isPointer) {
          val name = ctx.nameFor(x)
          ctx.makePointer(x)
          ctx << "auto" << name << "=" << "std::make_shared<" << ctx.typeFor(
            ty,
            Some(x)
          ) << ">("
          value.foreach { x => assignmentRhs(ty, "", x) }
          ctx << ");"
        } else {
          ctx << ctx.typeFor(ty, Some(x)) << ctx.nameFor(x)
          ctx << value.foreach { x => assignmentRhs(ty, "=", x) } << ";"
        }
      case x: EirMultiDeclaration =>
        val tmp = value.map(value => {
          (value, x.children.map(_.name).mkString("_") + "_value_")
        })
        tmp match {
          case Some((expr, name)) => ctx << "auto" << name << "=" << expr << ";"
          case _                  =>
        }
        x.children.zipWithIndex.foreach { case (d, i) =>
          visitDeclarationLike(
            d,
            tmp.map(tmp => {
              val ty = ctx.resolve(d.declaredType)
              val value = CppNode({
                val getter = s"std::get<$i>(${tmp._2})"
                // do not move reference types -- but may need to be more conservative
                // to ensure parent is not a reference type?
                ty match {
                  case _: EirReferenceType => getter
                  case _                   => s"std::move(${getter})"
                }
              })
              value.foundType = Some(ty)
              value
            })
          )
        }
      case _ => ???
    }
  }

  override def visitMultiDeclaration(
      x: EirMultiDeclaration
  )(implicit ctx: CodeGenerationContext): Unit =
    visitDeclarationLike(x, x.initialValue)

  override def visitExtractorPattern(x: EirExtractorPattern)(implicit
      ctx: CodeGenerationContext
  ): Unit = ???
}
