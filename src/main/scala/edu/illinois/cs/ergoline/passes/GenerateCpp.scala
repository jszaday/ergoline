package edu.illinois.cs.ergoline.passes

import java.io.File
import java.nio.file.Paths

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirLambdaType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.{RichEirNode, RichEirResolvable, RichEirType}
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichOption, RichResolvableTypeIterable}
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends EirVisitor[CodeGenerationContext, Unit] {
  var visited : List[EirNode] = Nil
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = this.visit

  object GenCppSyntax {
    implicit class RichEirNode(self : EirNode) {
      def isSystem: Boolean =
        self.parent match {
          case Some(m: EirMember) => m.isSystem
          case _ => self.annotation("system").isDefined
        }
    }

    implicit class RichEirType(self : EirType) {
      def isPointer: Boolean = {
        self match {
          case _: EirLambdaType => true
          case _: EirClassLike | _: EirTemplatedType =>
            val cls = Find.asClassLike(self)
            val system: Option[EirAnnotation] = cls.annotation("system")
            system match {
              case Some(system) => system("pointer").exists(_.toBoolean)
              case None => !cls.isInstanceOf[EirProxy]
            }
          case _ => false
        }
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
          case _: EirLambdaType => false
          case _: EirClassLike => x.parent.exists(_.isInstanceOf[EirMember]) || x.annotation("transient").isDefined
          case t: EirType => Find.asClassLike(t).isTransient
        }
      }

      def equivalentTo(theirs: EirResolvable[EirType]): Boolean = {
        (Find.uniqueResolution(self), Find.uniqueResolution(theirs)) match {
          case (EirTemplatedType(_, ourBase, ourArgs), EirTemplatedType(_, theirBase, theirArgs)) =>
            ourBase.equivalentTo(theirBase) && (ourArgs.length == theirArgs.length) &&
              ourArgs.zip(theirArgs).forall(x => x._1.equivalentTo(x._2))
          case (x, y) => x == y
        }
      }
    }
  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    visitTemplateArgs(ctx, x.templateArgs)
    ctx << s"struct ${ctx.nameFor(x)};"

    ProxyManager.proxiesFor(x).foreach(p => {
      visitTemplateArgs(ctx, p.templateArgs)
      ctx << s"struct ${ctx.nameFor(p)};"
    })
  }

//  override def visitNamespace(ctx: CodeGenerationContext, node: EirNamespace): Unit = {
//    val parted = node.children.partition(_.isInstanceOf[EirClassLike])
//    node.children = parted._1.map(_.asInstanceOf[EirClassLike]).sorted ++ parted._2
//    super.visitNamespace(ctx, node)
//  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    x.namespaces.foreach(ns =>
      ctx << s"namespace" << ctx.nameFor(ns) << "{" << "struct" << ctx.nameFor(x) << ";" << "}")
  }

  def registerPolymorphs(ctx: CodeGenerationContext,
                         checked: Map[EirSpecializable, List[EirSpecialization]],
                         lambdas: Map[EirNamespace, List[EirLambdaExpression]]): Unit = {
    val global = Some(EirGlobalNamespace)
    val puppables = checked.keys.filter({
      case _: EirProxy => false
      case x: EirClassLike => !(x.annotation("system").isDefined || x.isAbstract || x.isTransient)
      case _ => false
    })

    ctx << "void enroll_polymorphs(void)" << "{"
    ctx << "hypercomm::detail::initialize();"
    puppables.foreach(x => {
      if (x.templateArgs.isEmpty) {
        ctx << "hypercomm::enroll<" << ctx.nameFor(x, global) << ">" << "()" << ";"
      } else {
        checked(x).foreach(y => {
          ctx.specialize(x, y)
          ctx << "hypercomm::enroll<" << ctx.nameFor(x, global) << ">" << "()" << ";"
          ctx.leave(y)
        })
      }
    })

    lambdas.flatMap(_._2).map(x => {
      ctx << "hypercomm::enroll<" << ctx.nameFor(x, global) << ">" << "()" << ";"
    })

    ctx << "}"
  }

  override def error(ctx: CodeGenerationContext, node : EirNode): Unit = ()

  private def arrayMember(ctx: CodeGenerationContext, x: Option[EirNode]): Option[String] = {
    asMember(x).collect{
      case m: EirMember if isArray(ctx, m.base) => m.name
    }
  }

  override def visitScopedSymbol[A <: EirNode](ctx: CodeGenerationContext, x: EirScopedSymbol[A]): Unit = {
    // TODO handle self applications :3
    val found = disambiguate(ctx, x)
    arrayMember(ctx, Some(found)) match {
      case Some("size") =>
        arrayDim(ctx, ctx.typeOf(x.target)) match {
          case Some(1) => ctx << x.target << "->shape[0]"
          case Some(n) if n > 0 => ctx << "std::tuple_cat(" << x.target << "->shape)"
          case _ => Errors.unreachable()
        }
      case _ =>
        val targetTy: EirType = ctx.exprType(x.target)
        ctx << x.target << fieldAccessorFor(targetTy) << ctx.nameFor(found)
    }
  }

  override def visitLambdaType(ctx: CodeGenerationContext, x: types.EirLambdaType): Unit = {
    ctx << "auto"
  }

  override def visitProxyType(ctx: CodeGenerationContext, x: types.EirProxyType): Unit =
    ctx << ctx.nameFor(ctx.resolve(x))

  override def visitImport(ctx: CodeGenerationContext, x: EirImport): Unit = {
//    if (x.wildcard || x.qualified.length == 1) ctx << s"using namespace ${(if (x.wildcard) x.qualified.init else x.qualified) mkString "::"};"
//    else ctx << s"using ${x.qualified.last} = ${x.qualified mkString "::"};"
  }

  def isEntryArgument(f: EirFunctionArgument): Boolean = {
    f.parent.flatMap(_.parent).flatMap(_.parent).exists(_.isInstanceOf[EirProxy])
  }

  def splitIndex(ctx: CodeGenerationContext, t: EirType, curr: String): Iterable[String] = {
    arrayDim(ctx, t) match {
      case Some(n) => (0 until n).map(idx => s"$curr[$idx]")
      case _ => Errors.unreachable()
    }
  }

  def castToPuppable(ctx: CodeGenerationContext, expr: EirExpressionNode, ours: EirType): Unit = {
    if (ours.isTransient) {
      Errors.cannotSerialize(expr, ours)
    } else {
      ctx << expr
    }
  }

  def visitCallArgument(ctx: CodeGenerationContext)(t: (EirExpressionNode, EirFunctionArgument)): CodeGenerationContext = {
    val theirs = t._2.declaredType.resolve().headOption
    (ctx.exprType(t._1), theirs) match {
      case (a: EirProxy, Some(b: EirProxy)) if a.isDescendantOf(b) =>
        ctx << ctx.nameFor(b) << "(" << t._1 << ")"
      case (a, Some(_)) if isEntryArgument(t._2) =>
        ctx << castToPuppable(ctx, t._1, a)
      case _ => ctx << t._1
    }
  }

  def visitArguments(ctx: CodeGenerationContext)(disambiguation: Option[EirNode], args: List[EirExpressionNode]): CodeGenerationContext = {
    // TODO add support for expansions
    val theirs: List[EirFunctionArgument] =
      asMember(disambiguation) match {
        // TODO this should drop args for new~!
        case Some(m@EirMember(_, f: EirFunction, _)) => f.functionArgs
        case _ => Nil
      }

    if (theirs.length == args.length) {
      val zipped = args.zip(theirs)

      if (zipped.nonEmpty) {
        zipped.init.foreach(pair => {
          visitCallArgument(ctx)(pair)
          ctx << ","
        })

        visitCallArgument(ctx)(zipped.last)
      } else {
        ctx
      }
    } else {
      ctx << (args, ",")
    }
  }

  def visitCallback(ctx: CodeGenerationContext, target: EirExpressionNode, isReduction: Boolean): Unit = {
    target match {
      case EirScopedSymbol(_proxy, _) =>
        val proxy = _proxy.foundType.to[EirProxy]
        val found = asMember(Some(disambiguate(ctx, target)))
        if (proxy.isDefined && found.exists(_.isEntry)) {
          ctx << "CkCallback("
          ctx << GenerateProxies.indexFor(ctx, proxy.get, found.map(_.member).to[EirFunction].get)
          ctx << "," << _proxy << ")"
        } else {
          Errors.expectedCallback(target)
        }
      case _ => Errors.expectedCallback(target)
    }
  }

  def visitReducer(ctx: CodeGenerationContext, _target: EirExpressionNode): Unit = {
    val target = asMember(Some(disambiguate(ctx, _target)))
    val annotation = target.flatMap(_.annotation("system")).flatMap(_("reducer"))
    annotation match {
      case Some(l@EirLiteral(_, EirLiteralTypes.String, _)) => ctx << l.stripped
      case _ => Errors.expectedReducer(_target)
    }
  }

  def handleOptionMember(ctx: CodeGenerationContext, m: EirMember, base: EirExpressionNode, args: List[EirExpressionNode]): Unit = {
    val apl = m.name == "apply"
    val opt = base match {
      case s: EirScopedSymbol[_] => s.target
      case _ if apl => base
      case _ => Errors.unreachable()
    }
    val rsv = if (apl) assertValid[EirLambdaType](ctx.typeOf(opt)).to else ctx.typeOf(opt)
    val ty = ctx.resolve(rsv) match {
      case t: EirTemplatedType if t.args.length == 1 => ctx.resolve(t.args.head)
      case _ => Errors.unreachable()
    }
    val ptr = ty.isPointer
    val astr = Option.unless(ptr)("*")
    def wrap(ty: EirType): Option[String] =
      Option.unless(ty.isPointer)(s"std::make_shared<${ctx.typeFor(ty, Some(base))}>")
    m.name match {
      case "get" => ctx << "(" << astr << opt << ")"
      case "apply" if args.isEmpty => ctx << "nullptr"
      case "apply" if args.nonEmpty =>
        if (ptr) ctx << args.head
        else ctx << wrap(ty) << "(" << args.head << ")"
      case "nonEmpty" | "isEmpty" => ctx << "(" << opt << (if (m.name == "nonEmpty") "!=" else "==") << "nullptr" << ")"
      case "getOrElse" => ctx << "(" << opt << "?" << astr << opt << ":" << args.head << ")"
      case "map" | "flatMap" =>
        val wrapped =
          Option.when(m.name == "map")(ctx.resolve(assertValid[EirLambdaType](ctx.typeOf(args.head)).to)).flatMap(wrap)
        ctx << "(" << opt << "?" << wrapped << "((*" << args.head << ")("<< astr << opt << "))" << ":" << "nullptr" << ")"
      case _ => ???
    }
  }

  def isOption(t: EirType): Boolean =
    t match {
      case t: EirClass => (t.name == "option") && (t.parent == globals.ergolineModule)
      case _ => false
    }

  def isOption(t: Option[EirNode]): Boolean = t.to[EirType].exists(isOption)

  def visitSystemCall(ctx: CodeGenerationContext, target: EirExpressionNode,
                      disambiguated: EirNode, args: List[EirExpressionNode]): Unit = {
    val base = target match {
      case f: EirScopedSymbol[_] => f.target
      case _ => target
    }
    val proxy = disambiguated.parent.to[EirProxy]
    val system = disambiguated.annotation("system").get
    val static = system("static").exists(_.toBoolean)
    val invert = system("invert").exists(_.toBoolean)
    val invOp = if (invert) "!" else ""
    val cast = system("cast").exists(_.toBoolean)
    val name = system("alias").map(_.stripped).getOrElse(ctx.nameFor(disambiguated))
    disambiguated.asInstanceOf[EirNamedNode] match {
      case m@EirMember(Some(_: EirProxy), _, _) if m.name == "contribute" =>
        ctx << "ergoline::contribute(this," << {
          visitCallback(ctx, args match {
            case List(value, reducer, target) =>
              ctx << value << "," << visitReducer(ctx, reducer) << ","
              target
            case List(target) => target
            case _ => Errors.unreachable()
          }, isReduction = true)
        } << ")"
      case _ : EirMember if proxy.isDefined =>
        name match {
          case "index" =>
            proxy.flatMap(_.collective) match {
              case Some(ProxyManager.arrayPtn(dim)) =>
                val tup = s"tuple<${List.fill(dim.toInt)("int") mkString ","}>"
                if (dim != "1") {
                  ctx << s"([&](int *idx) -> std::$tup { return std::make_$tup(${
                    (0 until dim.toInt).indices.map("std::forward<int>(idx[" + _ + "])") mkString ","
                  });})(const_cast<int*>"
                }
                ctx << "(" << base << ".ckGetIndex().data()" << (if (dim == "1") "[0])" else "))")
              case Some("nodegroup" | "group") => ctx << "(" << base << ".ckGetGroupPe())"
              case _ => error(ctx, target)
            }
          case "parent" => ctx << s"(CProxy_${proxy.get.baseName}(" << base << {
            proxy.get.collective match {
              case Some("group" | "nodegroup") => ".ckGetGroupID()))"
              case Some(s) if s.startsWith("array") => ".ckGetArrayID()))"
            }
          }
          case _ => error(ctx, target)
        }
      case m: EirMember if isOption(disambiguated.parent) => handleOptionMember(ctx, m, target, args)
      case _ : EirMember if static => ctx << s"$name(" << {
        visitArguments(ctx)(Some(disambiguated), base +: args)
      } << ")"
      case EirMember(_, f: EirFunction, _) if cast =>
        ctx << s"((" << ctx.typeFor(f.returnType) << ")" << base << ")"
      case m : EirMember =>
        if (name == "apply") ctx << base << s"(" << visitArguments(ctx)(Some(disambiguated), args) << ")"
        else {
          val fqnOrDot = if (m.isStatic) "::" else fieldAccessorFor(ctx.exprType(base))
          ctx << invOp << base << s"$fqnOrDot$name(" << visitArguments(ctx)(Some(disambiguated), args) << ")"
        }
      case _ : EirFunction if name == "CkPrintf" || name == "CkAbort" =>
        ctx << name << "(\"%s\\n\"," << "(" << {
          visitArguments(ctx)(Some(disambiguated), args)
        } << ")" << ".c_str())"
      case _ => ctx << s"($name(" << visitArguments(ctx)(Some(disambiguated), args) << "))"
    }
  }

  def disambiguate(ctx: CodeGenerationContext, x: EirExpressionNode): EirNode = {
    x.disambiguation.getOrElse(x match {
      case x: EirResolvable[_] => ctx.resolve(x)
      case x => x
    })
  }

  def visitSpecialization(ctx: CodeGenerationContext, s: EirSpecialization): Unit = {
    if (s.types.nonEmpty) {
      val types = s.types.map(ctx.resolve[EirType])
      ctx << "<" << {
        for (t <- types.init) {
          ctx << ctx.typeFor(t)
          ctx << ","
        }
      } << ctx.typeFor(types.last) << ">"
    }
  }

  override def visitFunctionCall(ctx: CodeGenerationContext, x: EirFunctionCall): Unit = {
    val disambiguated = disambiguate(ctx, x.target)
    val member = asMember(Some(disambiguated))
    val isAsync = disambiguated.annotation("async").isDefined
    val shouldPack = member.exists {
      case m@EirMember(Some(_: EirProxy), _, _) => (x.args.nonEmpty || isAsync) && (m.isEntry || m.isMailbox)
      // TODO this should be a local call (that does not involve packing!)
      case m: EirMember => x.args.nonEmpty && m.isEntryOnly
    }
    val arrayAccessor = member.collect{
      case m: EirMember if isArray(ctx, m.base) => m.name
    }
    // bypass arguments for size (implicit field accessor)
    if (arrayAccessor.contains("size")) {
      ctx << x.target
      return
    }
    if (disambiguated.isSystem) {
      ctx << visitSystemCall(ctx, x.target, disambiguated, x.args)
    } else {
      if (isAsync) {
        val retTy = disambiguated match {
          case EirMember(_, f: EirFunction, _) => ctx.resolve(f.returnType)
          case f: EirFunction => ctx.resolve(f.returnType)
          case _ => Errors.missingType(disambiguated)
        }
        ctx << "(([&](){" << ctx.typeFor(retTy) << ctx.temporary << ";"
      }
      val isPointer = x.target match {
        // TODO make this more robust
        case s: EirSymbol[_] => ctx.resolve(s) match {
          case _: EirDeclaration => true
          case EirMember(_, _: EirDeclaration, _) => true
          case _: EirFunctionArgument => true
          case _ => false
        }
        case _ => false
      }
      if (isPointer) ctx << "(*"
      ctx << x.target
      if (isPointer) ctx << ")"
      ctx << visitSpecialization(ctx, x) << "(" << {
        if (shouldPack) ctx << "ergoline::pack("
        if (isAsync) {
          ctx << ctx.temporary
          if (x.args.nonEmpty) ctx << ","
        }
        visitArguments(ctx)(Some(disambiguated), x.args) << Option.when(shouldPack)(")")
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

  override def visitForLoop(ctx: CodeGenerationContext, x: EirForLoop): Unit = {
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
  }

  override def visitWhileLoop(ctx: CodeGenerationContext, x: EirWhileLoop): Unit = {
    ctx << s"while (" << x.condition << ")" << x.body
  }

  override def visitLiteral(ctx: CodeGenerationContext, x: EirLiteral): Unit = {
    if (x.`type` == EirLiteralTypes.String) ctx << s"std::string(${x.value})"
    else ctx << x.value
  }

  def selfFor(ctx: CodeGenerationContext, x: EirMember): String = {
    ctx.proxy.collect({
      case _ if x.isImplOnly => "impl_"
    }).getOrElse("this")
  }

  def asMember(x: Option[EirNode]): Option[EirMember] = {
    x match {
      case Some(m: EirMember) => Some(m)
      case _ => x.flatMap(_.parent).to[EirMember]
    }
  }

  override def visitSymbol[A <: EirNamedNode](ctx: CodeGenerationContext, x: EirSymbol[A]): Unit = {
    if (!CheckTypes.isSelf(x)) {
      val m = asMember(x.disambiguation)
      if (!m.exists(_.isStatic)) m.foreach(ctx << selfFor(ctx, _) << "->")
    }
    val m = asMember(x.disambiguation) match {
      case Some(m) if m.isEntryOnly => Some(Find.namedChild[EirMember](ctx.proxy, m.name))
      case _ => None
    }
    ctx << m.map(ctx.nameFor(_)).getOrElse(ctx.nameFor(x, Some(x)))
  }

  override def visitDeclaration(ctx: CodeGenerationContext, x: EirDeclaration): Unit = {
    ctx << ctx.typeFor(x.declaredType, Some(x)) << s"${ctx.nameFor(x)}" << x.initialValue.map(_ => "=") << x.initialValue << ";"
  }

  override def visitTemplateArgument(ctx: CodeGenerationContext, x: EirTemplateArgument): Unit = {
    ctx << s"typename ${x match {
      case _ if x.isPack => s"... ${x.name}"
      case _ => x.name
    }}"
  }

  def visitInherits(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    val parents = (x.implementsThese ++ x.extendsThis).map(ctx.resolve).map(ctx.nameFor(_))
    if (x.isInstanceOf[EirTrait]) {
      ctx << {
        if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(",") else ": public ergoline::object"
      }
    } else {
      ctx << ": " << {
        if (parents.isEmpty) "public ergoline::object" else parents.map("public " + _).mkString(",")
      } << {
        val hasPupableParent = x.extendsThis.map(ctx.resolve[EirType]).exists(!_.isTrait)
        Option.unless(x.isTransient || hasPupableParent)(", public hypercomm::polymorph")
      } << {
        ", public std::enable_shared_from_this<" + nameFor(ctx, x, includeTemplates = true) +">"
      }
    }
  }

  def puppingParents(ctx: CodeGenerationContext, x: EirClassLike): List[EirType] = {
    // TODO enable stateful traits?
    // TODO be templating aware?
    (x.extendsThis ++ x.implementsThese).map(ctx.resolve[EirType])
      .filterNot(_.isTrait).toList
  }

  def pupperFor(ctx: (CodeGenerationContext, EirNode, String))(current: String, ours: EirType): List[String] = {
    List(s"${ctx._3} | $current;")
  }

  def makePupper(ctx: CodeGenerationContext, x: EirClassLike, isMember: Boolean = false): Unit = {
    // TODO check to ensure user does not override
    val puper = ctx.temporary
    val header = if (isMember) s"virtual void __pup__(hypercomm::serdes &$puper) override" else s"void ${ctx.nameFor(x)}::__pup__(hypercomm::serdes &$puper)"
    ctx << header << "{" << {
      val parents = puppingParents(ctx, x).map(ctx.nameFor(_)).map(_ + s"::__pup__($puper);")
      val values = x.members.collect({
        case m@EirMember(_, d: EirDeclaration, _) if m.annotation("transient").isEmpty && !m.isStatic => d
      }).flatMap(d => {
        pupperFor((ctx, x, puper))(ctx.nameFor(d), ctx.resolve(d.declaredType))
      })
      parents ++ values
    } << s"}"
  }

  def hasherFor(ctx: CodeGenerationContext, d: EirDeclaration, hasher: String): Unit = {
    ctx << s"$hasher | ${ctx.nameFor(d)};"
  }

  def makeHasher(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    val hasher = ctx.temporary
    ctx << s"virtual std::size_t hash() override" << "{"
    ctx << "ergoline::hasher" << hasher << ";"
    // TODO combine with hash of parents
    val members = x.members.collect({
      // TODO should we consider transient or nah?
      case m@EirMember(_, d: EirDeclaration, _) if m.annotation("hashExclude").isEmpty && !m.isStatic => d
    })
    if (members.nonEmpty) {
      members.foreach(hasherFor(ctx, _, hasher))
    } else {
      ctx << s"$hasher | typeid(this).hash_code();"
    }
    ctx << s"return $hasher.hash();"
    ctx << "}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    if (x.templateArgs.isEmpty) {
      val isSystem = x.annotation("system").isDefined
      ctx << x.members.filter(_.member.isInstanceOf[EirFunction])
      if (!x.isTrait && !x.isTransient && !isSystem && !GenerateDecls.hasPup(x)) {
        makePupper(ctx, x)
      }
    }
  }

  override def visitMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
    ctx << Option.when(x.isStatic)("static")
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: CodeGenerationContext, args : List[EirTemplateArgument]): Unit = {
    if (args.nonEmpty) ctx << s"template<" << (args, ",") << "> "
  }

  def generateAssignments(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx << x.functionArgs.filter(_.isSelfAssigning).map(x => {
      "this->" + x.name + "=" + x.name + ";"
    })
  }

  def visitFunctionBody(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    val member = x.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val declarations = Option.when(member.exists(_.isConstructor))(
      parent.map(_.members).getOrElse(Nil).map(_.member).collect({
        case d: EirDeclaration if d.initialValue.isDefined => d
      })).toIterable.flatten
    val currSelf = ctx.proxy.map(_ => "impl_").getOrElse("this")
    ctx << {
      val assignments = x.functionArgs.filter(_.isSelfAssigning)
      if (assignments.nonEmpty || declarations.nonEmpty) {
        ctx << "{"
        ctx.ignoreNext("{")
      }
      assignments.map(arg => {
        val name = ctx.nameFor(arg)
        currSelf + "->" + name + "=" + name + ";"
      })
    } << {
      declarations.foreach(d => {
        ctx << currSelf << "->" << ctx.nameFor(d) << "=" << d.initialValue << ";"
      })
    } <| (x.body, ";")
  }

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction, isMember: Boolean): Unit = {
    val member = x.parent.to[EirMember]
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val entryOnly = member.exists(_.isEntryOnly) && ctx.proxy.isEmpty
    val system = member.flatMap(_.annotation("system")).orElse(x.annotation("system")).isDefined
    val abstractMember = !isMember && (parent.exists(_.isAbstract) && x.body.isEmpty)
    val langCi = ctx.language == "ci"
    val isTempl = parent.isDefined && !isMember && x.templateArgs.nonEmpty
    if ((!langCi && entryOnly) || system || abstractMember || isTempl) {
      return
    }
    val asyncCi = langCi && isMember && member.flatMap(_.annotation("async")).isDefined
    val isConstructor = member.exists(_.isConstructor)
    val overrides = Option.when(isMember && member.exists(_.isOverride))(" override")
    val name = parent match {
      case Some(p : EirProxy) if langCi && isConstructor => p.baseName
      case Some(classLike) if !isMember => ctx.nameFor(classLike) + "::" + ctx.nameFor(x)
      case _ => ctx.nameFor(x)
    }
    val virtual = Option.when(isMember && !langCi && member.exists(_.isVirtual))("virtual")
    visitTemplateArgs(ctx, x.templateArgs)
    ctx << virtual
    // TODO add templates when !isMember
    if (asyncCi) {
      ctx << "void"
    } else {
      ctx << Option.when(!isConstructor)(ctx.typeFor(x.returnType))
    }
    val args = x.functionArgs
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
      } else if (langCi || (!parent.exists(_.templateArgs.nonEmpty) && x.templateArgs.isEmpty)) {
        ctx << ";"
        return
      }
    } else if (x.body.isEmpty) {
      Errors.missingBody(x)
    }
    visitFunctionBody(ctx, x)
  }

  override def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit = visitFunction(ctx, x, isMember = false)

  def visitAnnotations(ctx: CodeGenerationContext, annotations: Iterable[EirAnnotation]): Unit = {
    ctx << "/* " << (annotations.map(_.toString), " ") << " */ "
  }

  override def visitBinaryExpression(ctx: CodeGenerationContext, x: EirBinaryExpression): Unit = {
    val target = x.disambiguation.collect {
      case EirFunctionCall(_, f: EirScopedSymbol[_], _, _) => f.disambiguation
    }.flatten
    val isSystem = target.forall(_.isSystem)
    ctx << Option.when(isSystem)("(") << x.lhs
    if (isSystem) {
      ctx << {
        if (x.op == "===" || x.op == "!==") x.op.init
        else x.op
      }
    } else {
      ctx << "->" << target.to[EirNamedNode].map(_.name) << "("
    }
    ctx << x.rhs << ")"
  }

  def qualifiedNameFor(ctx: CodeGenerationContext, _usage: EirNode, includeTemplates: Boolean = false)(of: EirNode): String = {
    val usage = Some(_usage)
    val base = of match {
      case t: EirTemplatedType => ctx.resolve(t.base)
      case _ => of
    }
    if (base.annotation("system").isDefined) {
      return nameFor(ctx, of, includeTemplates, usage)
    }
    val ours = usage.flatMap(Find.parentOf[EirNamespace])
    val theirs = Find.parentOf[EirNamespace](base)
    val qualifications: Seq[String] =
      theirs match {
        case Some(theirs) if !ours.contains(theirs) =>
          (theirs.name +: Find.ancestors(theirs).collect{
            case n: EirNamespace => n.name
          }).reverse
        case _ => Nil
      }
    (qualifications :+ nameFor(ctx, of, includeTemplates, usage)).mkString("::")
  }

  def templateArgumentsToString(ctx: CodeGenerationContext, args: List[EirResolvable[EirType]], usage: Option[EirNode]): String = {
    "<" + {
      args.map(ctx.resolve[EirType]).map(ctx.typeFor(_, usage)).mkString(",")
    } + ">"
  }

  def selfIndex(p: Option[EirProxy]): String = {
    p.flatMap(_.collective) match {
      case Some("nodegroup") => "CkMyNode()"
      case Some("group") => "CkMyPe()"
      case Some(s) if s.startsWith("array") => "this->thisIndex"
      case _ => Errors.unreachable()
    }
  }

  def selfName(ctx: CodeGenerationContext, s: EirSymbol[_]): String = {
    s.qualifiedName.last match {
      case "self@" => "this->thisProxy"
      case "self[@]" => s"this->thisProxy[${selfIndex(ctx.proxy)}]"
      case _ => selfName(ctx, s.asInstanceOf[EirNode])
    }
  }

  def selfName(ctx: CodeGenerationContext, n: EirNode): String = {
    val ty = n match {
      case e: EirExpressionNode => ctx.exprType(e)
      case d: EirDeclaration => ctx.resolve(d.declaredType)
      case EirMember(_, d: EirDeclaration, _) => ctx.resolve(d.declaredType)
      case _ => Errors.missingType(n)
    }
    "(" + nameFor(ctx, ty, includeTemplates = true) + "::shared_from_this())"
  }

  def nameFor(ctx: CodeGenerationContext, x : EirNode, includeTemplates: Boolean = false, usage : Option[EirNode] = None): String = {
    val alias =
      x.annotation("system").flatMap(_("alias")).map(_.stripped)
    val dealiased = alias.orElse(x match {
      case n: EirNamedNode => Some(n.name)
      case _ => None
    }).map(x => {
      if (x == "std::size_t" && ctx.language == "ci") "size_t"
      else x
    })
    val opt = Some(x)
    val proxy = opt.to[EirType].flatMap(ProxyManager.asProxy)
        .orElse(asMember(opt).flatMap(_.parent.to[EirProxy]))
    val result = x match {
      case s: EirSymbol[_] =>
        if (CheckTypes.isSelf(s)) {
          selfName(ctx, s)
        } else {
          // TODO need to use FQN here, symbol is self-context providing
          nameFor(ctx, ctx.resolve(s), includeTemplates, usage.orElse(Some(s)))
        }
      case _: EirMember | _: EirFunction if proxy.isDefined =>
        (x, proxy.flatMap(_.ordinalFor(x))) match {
          case (x: EirNamedNode, Some(ord)) => s"__${x.name}_${ord}__"
          case (x: EirNamedNode, _) => x.name
          case (_, _) => Errors.unreachable()
        }
      case _ if proxy.isDefined =>
        val prefix =
          if (proxy.get.isElement) "CProxyElement_" else "CProxy_"
        val name = prefix + proxy.get.baseName
        x match {
          case t: EirTemplatedType => name + templateArgumentsToString(ctx, t.args, usage)
          case _ => name
        }
      case _ if dealiased.contains("self") => selfName(ctx, x)
      case x: EirLambdaType =>
        val args = (x.to +: x.from).map(ctx.typeFor(_, Some(x)))
        s"ergoline::function<${args.mkString(",")}>"
      case x: EirTemplateArgument =>
        (ctx.hasSubstitution(x) match {
          case Some(t) => ctx.nameFor(t)
          case None => dealiased.get
        }) + (if (x.isPack) "..." else "")
      case x: EirTemplatedType =>
        arrayDim(ctx, x) match {
          case Some(n) => s"ergoline::array<${ctx.typeFor(arrayElementType(x), usage)}, $n>"
          case None =>  nameFor(ctx, ctx.resolve(x.base), usage=usage) + templateArgumentsToString(ctx, x.args, usage)
        }
      case x: EirSpecializable with EirNamedNode if x.templateArgs.nonEmpty =>
        val subst = x.templateArgs.map(ctx.hasSubstitution)
        val substDefined = subst.forall(_.isDefined)
        dealiased.get + (if (includeTemplates || substDefined) {
           "<" + {
             if (substDefined) subst.flatten.map(ctx.typeFor(_, usage)) else x.templateArgs.map(ctx.typeFor(_, usage))
           }.mkString(",") + ">"
        } else "")
      case t: EirTupleType =>
        s"std::tuple${templateArgumentsToString(ctx, t.children, usage)}"
      case _: EirNamedNode => dealiased.get
      case s: EirConstantFacade => s.value.value
      case x: EirLambdaExpression =>
        _lambda_names.get(x) match {
          case Some(name) => name
          case None =>
            val name = x.location
              .map((i: EirSourceInfo) => s"__lambda__${((s: String) => {
                s.substring(0, s.indexOf('.'))
              })(Paths.get(i.sourceName).getFileName.toString)}__L${i.line}C${i.start}__")
              .getOrElse(Errors.unableToName(x))
            _lambda_names.put(x, name)
            name
        }
    }
    if (ctx.hasPointerOverride(x)) s"(*$result)" else result
  }

  def typeForEntryArgument(ctx: (CodeGenerationContext, EirNode))(ty: EirResolvable[EirType]): String = {
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
      case t: EirLambdaType => ctx._1.typeFor(t, Some(ctx._2))
      case t: EirType if t.isTransient => Errors.cannotSerialize(ctx._2, t)
      // TODO exempt system types here?
      case t: EirType if t.isTrait => "std::shared_ptr<PUP::able>"
      case t: EirType => ctx._1.typeFor(t, Some(ctx._2))
      case _ => Errors.incorrectType(resolution, classOf[EirType])
    }
  }

  override def visitFunctionArgument(ctx: CodeGenerationContext, x: EirFunctionArgument): Unit = {
    ctx << {
      if (isEntryArgument(x)) typeForEntryArgument((ctx, x))(x.declaredType)
      else ctx.typeFor(x.declaredType, Some(x))
    } << ctx.nameFor(x)
  }

  override def visitTupleExpression(ctx: CodeGenerationContext, x: EirTupleExpression): Unit = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    ctx << s"std::$func(" << (x.children, ",") << ")"
  }

  private val _lambda_names: mutable.Map[EirLambdaExpression, String] = mutable.Map()

  override def visitLambdaExpression(ctx: CodeGenerationContext, x: EirLambdaExpression): Unit = {
    val captures = x.captures.map(captured => {
      // TODO use specialized version when avail
      val ty = ctx.typeOf(captured)
      val name = ctx.nameFor(captured, Some(x))
      if (ty.isPointer) name else {
        val t = ctx.typeFor(ty, Some(x))
        s"std::shared_ptr<$t>(std::shared_ptr<$t>{}, &$name)"
      }
    })
    ctx << s"std::make_shared<${ctx.nameFor(x)}>(${captures mkString ","})"
  }

  def makeLambdaWrapper(ctx: CodeGenerationContext, lambda: EirLambdaExpression): Unit = {
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
    ctx << s"$name(${
      cdecltypes.zipWithIndex.map({
        case (t, idx) => s"$t _$idx"
      }) mkString ","
    })" << Option.when(captures.nonEmpty)(": ") << {
      (captures.zipWithIndex.map({
        case (n, idx) => s"${n.name}(_$idx)"
      }), ",")
    } << "{ }"
    //    if () ctx << s"$name() { }"
    ctx << s"$name(PUP::reconstruct __tag__): ergoline::function<${args mkString ","}>(__tag__) { }"
    ctx << "virtual void __pup__(hypercomm::serdes& _) override" << "{"
    if (isTransient) {
      ctx << "CkAbort(\"lambda" << name << "is transient and cannot be pup'd.\""
    } else {
      ctx << captures.zip(ctypes).flatMap({
        case (n, t) => pupperFor((ctx, lambda, "_"))(n.name, t)
      })
    }
    ctx << "}"
    ctx << s"virtual" << args.head << "operator()("
    ctx << (lambda.args.zip(args.tail).map({ case (arg, ty) => s"$ty ${arg.name}" }), ",")
    ctx << ") override"
    val crefs = captures.zip(ctypes).collect{
      case (x, t) if !t.isPointer => x
    }
    crefs.foreach(ctx.makePointer)
    ctx << lambda.body
    crefs.foreach(ctx.unsetPointer)
    ctx << captures.zip(cdecltypes).map({
      case (n, t) => s"$t ${n.name};"
    })
    ctx << "};"
  }

  @tailrec
  def isArray(ctx: CodeGenerationContext, t: EirType): Boolean = {
    t match {
      case t: EirTemplatedType => isArray(ctx, ctx.resolve(t.base))
      case c: EirClass => c.name == "array" && c.parent == globals.ergolineModule
      case _ => false
    }
  }

  def containsArray(ctx: CodeGenerationContext, t: EirResolvable[EirType]): Boolean = {
    ctx.resolve(t) match {
      case t: EirTupleType => t.children.exists(containsArray(ctx, _))
      case t => isArray(ctx, t)
    }
  }

  def arrayDim(ctx: CodeGenerationContext, t: EirType): Option[Int] = {
    ctx.resolve(t) match {
      case t: EirTemplatedType if isArray(ctx, t) =>
        t.args match {
          case _ +: Nil => Some(1)
          case _ +: t +: Nil => Some(ctx.eval2const(t).toInt)
          case _ => None
        }
      case _ => None
    }
  }

  def arrayElementType(t: EirType): EirResolvable[EirType] = {
    t match {
      case t: EirTemplatedType => t.args.head
      case _ => Errors.unreachable()
    }
  }

  def makeIndex(ctx: CodeGenerationContext, args: List[EirExpressionNode]): Unit = {
    ctx << "{ (std::size_t) " << (args, ", (std::size_t) ") << "})"
  }

  override def visitNew(ctx: CodeGenerationContext, x: EirNew): Unit = {
    val objTy: EirType = ctx.resolve(x.target)
    val proxy = ProxyManager.asProxy(objTy)
    val numTake: Int = proxy.flatMap(_.collective)
      .find(_.startsWith("array"))
      .map(ProxyManager.dimensionality)
      .getOrElse(0)
    val args = x.args.drop(numTake)
    objTy match {
      case _ if proxy.isDefined =>
        ctx << "(" << "(" << ctx.nameFor(objTy, Some(x)) << ")"
        ctx << ctx.nameFor(objTy, Some(x)) << s"::ckNew("
        if (args.nonEmpty) {
          ctx << "ergoline::pack(" << {
            visitArguments(ctx)(x.disambiguation, args)
          } << ")"
        }
        if (numTake > 0) {
          ctx << Option.when(args.nonEmpty)(",")
          ctx << (x.args.slice(0, numTake), ",")
        }
        ctx << ")" << ")"
      case t: EirType if t.isPointer =>
        ctx << "std::make_shared<" << ctx.nameFor(t, Some(x)) << ">("
        arrayDim(ctx, t) match {
          case Some(n) =>
            ctx << "ergoline::array<" << ctx.typeFor(arrayElementType(t), Some(x)) << "," << n.toString << ">(" << makeIndex(ctx, args)
          case None => visitArguments(ctx)(x.disambiguation, args)
        }
        ctx<< ")"
      case _ => ctx << "new" << ctx.typeFor(objTy, Some(x)) << "(" << visitArguments(ctx)(x.disambiguation, args) << ")"
    }
  }

  override def visitMatch(ctx: CodeGenerationContext, x: EirMatch): Unit = {
    // TODO restore failure to match CmiAbort/throw!
    ctx << s"([&](" << ctx.typeFor(ctx.exprType(x.expression)) << s"${ctx.temporary}) ->" << ctx.typeFor(ctx.exprType(x)) << "{" << x.cases << {
      val location = Errors.contextualize(x)
      "CkAbort(\"no match found at " + location.substring(location.lastIndexOf(File.separator) + 1) + "\");"
    }<< "})(" << x.expression << ")"
  }

  def visitPatternDecl(parent: CodeGenerationContext, x: EirPattern, current: String): String = {
    val ctx = parent.makeSubContext()
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => ctx << visitPatternDecl(ctx, p, current)
        case patterns =>
          ctx << patterns.zipWithIndex.map({
            case (p, idx) => visitPatternDecl(ctx, p, s"std::get<$idx>($current)")
          })
      }
      case i@EirIdentifierPattern(_, n, t) if n != "_" =>
        val ty = ctx.resolve(t)
        ctx.ignoreNext(";")
        if (ty.isPointer) ctx << i.declarations.head << s" = std::dynamic_pointer_cast<${ctx.nameFor(t)}>($current);"
        else ctx << i.declarations.head << s" = $current;"
      case i: EirIdentifierPattern =>
        if (i.name != "_") Errors.missingType(x)
      case _: EirExpressionPattern =>
    }
    ctx.toString
  }

  def typeAt(ctx: CodeGenerationContext, t: Option[EirType], idx: Int): Option[EirType] = {
    t match {
      case Some(t: EirTupleType) if idx < t.children.length =>
        Some(ctx.resolve[EirType](t.children(idx)))
      case _ => None
    }
  }

  def visitPatternCond(parent: CodeGenerationContext, x: EirPattern, current: String, parentType: Option[EirType]): List[String] = {
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => visitPatternCond(parent, p, current, parentType)
        case patterns =>
          patterns.zipWithIndex.flatMap {
            case (p, idx) => visitPatternCond(parent, p, s"std::get<$idx>($current)", typeAt(parent, parentType, idx))
          }
      }
      case EirIdentifierPattern(_, "_", t) =>
        parentType match {
          case None => Errors.missingType(x)
            // TODO use a more reliable comparison here!
          case Some(u) if t.equivalentTo(u) => Nil
          case _ =>
            // TODO this needs to inherit substitutions
            //      (when such things are added)
            val ctx = parent.makeSubContext()
            List(s"std::dynamic_pointer_cast<${ctx.nameFor(t)}>($current)")
        }
      case EirIdentifierPattern(_, n, t) =>
        Option.when(parent.resolve(t).isPointer)(n).toList
      case e: EirExpressionPattern =>
        val ctx = parent.makeSubContext()
        ctx.putReplacement("_", current)
        (ctx << e.expression).toString.split(n).toList
    }
  }

  override def visitMatchCase(ctx: CodeGenerationContext, x: EirMatchCase): Unit = {
    val parent = x.parent.to[EirMatch]
    val exprType = parent.map(_.expression).map(ctx.exprType)
    val isUnit = parent.map(ctx.exprType)
      .contains(globals.typeFor(EirLiteralTypes.Unit))
    ctx << "{" << visitPatternDecl(ctx, x.patterns, ctx.temporary).split(n)
    val conditions = visitPatternCond(ctx, x.patterns, ctx.temporary, exprType).mkString(" && ")
    val needsIf = x.condition.nonEmpty || conditions.nonEmpty
    if (needsIf) ctx << "if(" << x.condition << {
      Option.when(x.condition.isDefined && conditions.nonEmpty)(" && ")
    } << conditions << ")" << "{"
    val (primary, secondary) = (Option.unless(isUnit)("return"), Option.when(isUnit)("return;"))
    ctx << primary << x.body << ";" << secondary << Option.when(needsIf)("}") << "}"
  }

  override def visitTupleType(ctx: CodeGenerationContext, x: types.EirTupleType): Unit = {
    ctx << s"std::tuple<"
    if (x.children.nonEmpty) {
      for (t <- x.children.init) {
        ctx << ctx.typeFor(t) << ","
      }
      ctx << ctx.typeFor(x.children.last)
    }
    ctx << ">"
  }

  override def visitArrayReference(ctx: CodeGenerationContext, arrayRef: EirArrayReference): Unit = {
    val ty = ctx.exprType(arrayRef.target)
    val collective = ProxyManager.asProxy(ty).flatMap(x => if (!x.isElement) x.collective else None)
    ty match {
      case tty : EirTupleType =>
        val arg = arrayRef.args.headOption.map(CheckTypes.evaluateConstExpr(ctx.typeContext, _))
        arg match {
          case Some(x) => ctx << s"std::get<" << x << ">(" << arrayRef.target << ")"
          case None => Errors.invalidTupleIndices(tty, arrayRef.args)
        }
      case _ if collective.exists(_.startsWith("array")) =>
        ctx << arrayRef.target << "(" << (arrayRef.args, ",") << ")"
      case _ if collective.exists(x => x == "group" || x == "nodegroup") =>
        ctx << arrayRef.target << "[" << (arrayRef.args, ",") << "]"
      case t if isArray(ctx, t) =>
        val target = arrayRef.target
        val args = arrayRef.args
        ctx << "(*" << target << ")["
        args.reverse.init.zipWithIndex.foreach {
          case (arg, idx) => ctx << arg << s"* (" << target << s"->shape[$idx]) +"
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

  override def visitSpecializedSymbol(ctx: CodeGenerationContext, x: EirSpecializedSymbol): Unit = {
    val base = ctx.resolve(x.symbol)
    ctx << ctx.nameFor(base) << visitSpecialization(ctx, x)
  }

  override def visitIfElse(ctx: CodeGenerationContext, x: EirIfElse): Unit = {
    ctx << "if (" << x.test << ")" << x.ifTrue << x.ifFalse.map(_ => "else ") << x.ifFalse
  }

  override def visitTernaryOperator(ctx: CodeGenerationContext, x: EirTernaryOperator): Unit = {
    ctx << "(" << x.test << "?" << x.ifTrue << ":" << x.ifFalse << ")"
  }

  override def visitTemplatedType(ctx: CodeGenerationContext, x: EirTemplatedType): Unit = {
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

  override def visitBlock(ctx: CodeGenerationContext, x: EirBlock): Unit = {
    ctx << "{"
    x.children.foreach{
      case x: EirExpressionNode => ctx << x << ";"
      case x => ctx << x
    }
    ctx << "}"
  }

  override def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace" << x.name << "{" << x.children << "}"
  }

  override def visitClass(ctx: CodeGenerationContext, x: EirClass): Unit = visitClassLike(ctx, x)

  override def visitTrait(ctx: CodeGenerationContext, x: EirTrait): Unit = visitClassLike(ctx, x)

  override def visitAnnotation(ctx: CodeGenerationContext, x: EirAnnotation): Unit = {
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

  override def visitAssignment(ctx: CodeGenerationContext, x: EirAssignment): Unit = {
    x.lval match {
      case x: EirArrayReference if !isPlainArrayRef(x) => ctx << x << ";"
      case _ => ctx << x.lval << x.op << x.rval << ";"
    }
  }

  override def visitReturn(ctx: CodeGenerationContext, x: EirReturn): Unit = {
    ctx << "return" << x.expression << ";"
  }

  override def visitAwait(ctx: CodeGenerationContext, x: EirAwait): Unit = {
    x.release match {
      case Some(_) =>
        val target = x.target
        val found = ctx.exprType(x.target)
        val tmp = ctx.temporary
        ctx << "([](" << ctx.typeFor(found, Some(x)) << tmp << ")" << "{"
        ctx << "auto" << "val" << "=" << tmp << fieldAccessorFor(found) << "get()" << ";"
        ctx <<  tmp << fieldAccessorFor(found) << "release()" << ";"
        ctx << "return" << "val" << ";"
        ctx << "}" << ")(" << target << ")"
      case None => ctx << x.disambiguation
    }
  }

  // TODO implement this?
  private def escapeInterpString(s: String) = "\"" + s + "\""

  override def visitInterpolatedString(ctx: CodeGenerationContext, str: EirInterpolatedString): Unit = {
    def matchChild(x: EirExpressionNode) = {
      x match {
        // TODO is this right?? Idk...
        case x if x.disambiguation.isDefined =>
          ctx << "(([&](){ return " << visit(ctx, x.disambiguation.get) << "; })())"
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

  override def visitPatternList(ctx: CodeGenerationContext, x: EirPatternList): Unit = ()
  override def visitIdentifierPattern(ctx: CodeGenerationContext, x: EirIdentifierPattern): Unit = ()
  override def visitExpressionPattern(ctx: CodeGenerationContext, x: EirExpressionPattern): Unit = ()
  override def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = ()
  override def visitTypeAlias(ctx: CodeGenerationContext, x: EirTypeAlias): Unit = ()
  override def visitTupleMultiply(context: CodeGenerationContext, multiply: types.EirTupleMultiply): Unit = ()
  override def visitConstantFacade(context: CodeGenerationContext, facade: EirConstantFacade): Unit = visit(context, facade.value)

  def makeSentinel(ctx: CodeGenerationContext, all: Boolean): String = {
    val sentinel = "__armin__"
    ctx << "const auto __self__ = CthSelf();"
    ctx << "ergoline::reqman "<< sentinel << "(" << all.toString << ");"
    sentinel
  }

  def makeCompoundRequest(ctx: CodeGenerationContext, x: EirSdagWhen): Unit = {
    ctx << "{"
    ctx << "const auto __self__ = CthSelf();"
    val patterns = EirPatternList(None, x.patterns.flatMap(_._2.patterns))
    val triples = x.patterns.zipWithIndex.map {
      case ((symbol, patterns), i) =>
        val m = Find.namedChild[EirMember](ctx.proxy, symbol.qualifiedName.last)
        val f = assertValid[EirFunction](m.member)
        val declTys = f.functionArgs.map(_.declaredType).map(ctx.resolve)
        val tys = declTys.map(ctx.typeFor(_, Some(x)))
        val name = GenerateProxies.mailboxName(ctx, f, tys)
        val conditions = visitPatternCond(ctx, patterns, "*" + ctx.temporary, Some(ctx.resolve(declTys.toTupleType(allowUnit = true)(None)))).mkString(" && ")
        val valueTy = s"decltype($name)::value_t"
        ctx << s"auto __request_${i}__ = this->$name.make_request(nullptr," << {
          if (conditions.nonEmpty) {
            s"[&]($valueTy ${ctx.temporary}) { return $conditions; });"
          } else "nullptr);"
        }
        (tys, name, s"__request_${i}__")
    }
    ctx << "std::shared_ptr<std::tuple<" << {
      triples.flatMap(_._1) mkString ","
    } << ">> __value__;"
    ctx << "auto __compound__ ="
    val len = x.patterns.length
    ctx << x.patterns.tail.indices.foldRight(s"__request_0__")((i, s) => {
      s"ergoline::join($s,__request_${i + 1}__," + {
        if (i < (len - 2)) {
          "nullptr,nullptr)"
        } else ""
      }
    })
    ctx << "[&](decltype(__value__) __recvd__) {"
    ctx << "__value__ = __recvd__;"
    ctx << "CthAwaken(__self__);"
    ctx << "return true;" << "}, nullptr);"
    triples.foreach{
      case (_, mbox, req) => ctx << s"this->$mbox.put($req);"
    }
    ctx << "CthSuspend();"
    ctx << {
      visitPatternDecl(ctx, patterns, "*__value__").split(n)
    } << {
      ctx.ignoreNext("{")
      ctx << x.body
    }
  }

  override def visitWhen(ctx: CodeGenerationContext, x: EirSdagWhen): Unit = {
    // TODO impl this
    if (x.condition.isDefined) ???
    if (x.patterns.length > 1) {
      makeCompoundRequest(ctx, x)
      return
    }
    x.patterns.foreach({
      case (symbol, patterns) => ctx << "{" << {
        val sentinel = makeSentinel(ctx, all = true)
        val m = Find.namedChild[EirMember](ctx.proxy, symbol.qualifiedName.last)
        val f = assertValid[EirFunction](m.member)
        val declTys = f.functionArgs.map(_.declaredType).map(ctx.resolve)
        val tys = declTys.map(ctx.typeFor(_, Some(x)))
        val name = "this->" + GenerateProxies.mailboxName(ctx, f, tys)
        val conditions = visitPatternCond(ctx, patterns, "*" + ctx.temporary, Some(ctx.resolve(declTys.toTupleType(allowUnit = true)(None)))).mkString(" && ")
        ctx << s"using value_t = typename decltype($name)::value_t;"
        ctx << s"auto __request__ = $name.make_request(nullptr,"
        ctx << {
          if (conditions.nonEmpty) {
            s"[&](const value_t& ${ctx.temporary}) { return $conditions; });"
          } else "nullptr);"
        }
        ctx << sentinel << ".put(__request__," << "[&](value_t __value__)" << "{"
        visitPatternDecl(ctx, patterns, "*__value__").split(n)
        ctx.ignoreNext("{")
        ctx << x.body
        ctx << ");"
        ctx << s"$name.put(__request__);"
        ctx << s"$sentinel.block();"
      } << "}"
    })
  }

  override def visitSlice(ctx: CodeGenerationContext, x: EirSlice): Unit = ???

  override def visitAwaitMany(ctx: CodeGenerationContext, x: EirAwaitMany): Unit = ???
}
