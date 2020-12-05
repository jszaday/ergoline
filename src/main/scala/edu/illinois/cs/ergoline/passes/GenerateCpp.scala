package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirProxyType, EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends UnparseAst {
  var visited : List[EirNode] = Nil

  object GenCppSyntax {
    implicit class RichEirType(eirType : EirType) {
      def isPointer: Boolean = eirType match {
        case x : EirTemplatedType =>
          Find.uniqueResolution[EirType](x.base).isPointer
        case x if x.annotation("system").isDefined => false
        case x : EirClassLike => !x.isInstanceOf[EirProxy]
        case _ => false
      }
    }
  }

  def typeFor(ctx: UnparseContext, x: EirResolvable[EirType], isEntryArgument: Boolean = false): String =
    typeFor(ctx, Find.uniqueResolution[EirType](x), isEntryArgument)

  def typeFor(ctx: UnparseContext, x: EirType, isEntryArgument: Boolean): String = {
    val name = nameFor(ctx, x)
    if (x.isPointer) {
      if (isEntryArgument) {
        x match {
          case c: EirClassLike if c.isAbstract => {
            if (ctx.lang == "ci") {
              "CkPointer<PUP::able>"
            } else {
              "PUP::able*"
            }
          }
          case _ => name + "*"
        }
      }
      else s"std::shared_ptr<$name>"
    }
    else name
  }

  def forwardDecl(ctx: UnparseContext, x: EirClassLike): String = {
    s"struct ${nameFor(ctx, x)};"
  }

//  override def visitNamespace(ctx: UnparseContext, node: EirNamespace): String = {
//    val parted = node.children.partition(_.isInstanceOf[EirClassLike])
//    node.children = parted._1.map(_.asInstanceOf[EirClassLike]).sorted ++ parted._2
//    super.visitNamespace(ctx, node)
//  }

  // TODO this will need to support templates
  def makeFromPuppable(ctx: UnparseContext, t: EirTrait): String = {
    def getDerived(t: EirTrait): Seq[EirClassLike] = {
      t.derived.partition(_.isInstanceOf[EirTrait]) match {
        case (traits, classes) => traits.map(_.asInstanceOf[EirTrait]).flatMap(getDerived) ++ classes
      }
    }
    val name = nameFor(ctx, t)
    val children = getDerived(t).toSet.map((c: EirClassLike) => {
      val otherName = nameFor(ctx, c)
      s"if (p->get_PUP_ID() == $otherName::my_PUP_ID) return ($otherName*) p;"
    }).toList :+ s"return nullptr;$n"
    s"$name* $name::fromPuppable(PUP::able *p) {" + {
      ctx.numTabs += 1
      val res = s"$n${ctx.t}" + children.mkString(s"$n${ctx.t}else ")
      ctx.numTabs -= 1
      res
    } + "}"
  }

//  def forwardDefn(ctx: UnparseContext, x: EirClassLike): String = {
//    s"${n}struct ${nameFor(ctx, x)}" + visitInherits(ctx, x) + "{" + {
//      ctx.numTabs += 1
//      val res = ""
//      ctx.numTabs -= 1
//      res
//    } + s"$n};"
//  }

  def forwardDecl(ctx: UnparseContext, x: EirProxy): String = {
    val ns = x.namespaces.toList
    (ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {") ++ {
      Seq(s"struct ${nameFor(ctx, x)};")
    } ++ ns.map(_ => "}")).mkString(n)
  }

  def error(ctx: UnparseContext, node : EirNode, msg: String): String = {
    s"/* skipped $node : $msg */"
  }

  override def error(ctx: UnparseContext, node : EirNode): String = error(ctx, node, "")

//  override def visitArrayReference(ctx: UnparseContext, x: EirArrayReference): String = ???

  override def visitFieldAccessor(ctx: UnparseContext, x: EirFieldAccessor): String = {
    // TODO handle self applications :3
    val targetTy: EirType = x.target.foundType.getOrElse(Errors.missingType(x.target))
    s"${visit(ctx, x.target)}${if (targetTy.isPointer) "->" else "."}${x.field}"
  }

  override def visitLambdaType(ctx: UnparseContext, x: types.EirLambdaType): String = "auto"

  override def visitProxyType(ctx: UnparseContext, x: types.EirProxyType): String =
    nameFor(ctx, Find.uniqueResolution(x))

  override def visitImport(ctx: UnparseContext, x: EirImport): String = ""

  def handleOption(ctx: UnparseContext, x : Option[EirNode]): Option[String] = {
    x.map({
      case n : EirNamedNode => nameFor(ctx, n)
      case n => visit(ctx, n)
    })
  }

  def isEntryArgument(f: EirFunctionArgument): Boolean = {
    f.parent.flatMap(_.parent).flatMap(_.parent).exists(_.isInstanceOf[EirProxy])
  }

  def visitCallArgument(ctx: UnparseContext)(t: (EirExpressionNode, EirFunctionArgument)): String = {
    val theirs = t._2.declaredType.resolve().headOption
    (t._1.foundType, theirs) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if a.isDescendantOf(b) =>
        s"${nameFor(ctx, b)}(${visit(ctx, t._1)})"
      case (Some(c: EirClassLike), Some(_)) if c.isPointer && isEntryArgument(t._2) =>{
        val templ = c.extendsThis.map(nameFor(ctx, _)).getOrElse("PUP::able")
        s"CkPointer<$templ>(${visit(ctx, t._1)}.get())"
      }
      case _ => visit(ctx, t._1)
    }
  }

  def visitArguments(ctx: UnparseContext)(disambiguation: Option[EirNode], args: List[EirExpressionNode]): List[String] = {
    val theirs: List[EirFunctionArgument] =
      disambiguation match {
        case Some(m@EirMember(_, f: EirFunction, _)) =>
          if (m.isStatic) f.functionArgs
          else f.functionArgs.drop(if (m.isConstructor && m.isEntry) 1 else 0)
        case Some(f: EirFunction) => f.functionArgs
        case _ => Nil
      }
    if (theirs.length == args.length) args.zip(theirs).map(visitCallArgument(ctx))
    else args.map(visit(ctx, _))
  }

  def visitSystemCall(ctx: UnparseContext, target: EirExpressionNode, disambiguated: EirNode, args: List[String]): String = {
    val base = target match {
      case f: EirFieldAccessor => f.target
      case _ => target
    }
    val proxy = disambiguated.parent.to[EirProxy]
    val system = disambiguated.annotation("system").get
    val name = system("alias").map(_.stripped).getOrElse(nameFor(ctx, disambiguated))
    val static = system("static").exists(_.value.toBoolean)
    disambiguated.asInstanceOf[EirNamedNode] match {
      case _ : EirMember if proxy.isDefined =>
        name match {
          case "index" =>
            proxy.flatMap(_.collective) match {
              case Some(ProxyManager.arrayPtn(dim)) =>
                val idx = s"${visit(ctx, base)}.ckGetIndex().data()"
                // TODO cast to tuple
                if (dim == "1") s"($idx[0])" else ???
              case Some("nodegroup" | "group") => s"(${visit(ctx, base)}.ckGetGroupPe())"
              case _ => error(ctx, target, "no generation method defined")
            }
          case "parent" => s"(CProxy_${proxy.get.baseName}(${visit(ctx, base)}.ckGetArrayID()))"
          case _ => error(ctx, target, "no generation method defined")
        }
      case _ : EirMember if static => s"$name(${(visit(ctx, base) +: args).mkString(", ")})"
      case _ : EirMember => s"${visit(ctx, base)}.$name(${args.mkString(", ")})"
      case f : EirFunction if f.name == "println" => "CkPrintf(\"%s\\n\", " + s"${args.map(x => x + ".c_str()").mkString(", ")})"
      case _ => s"($name(${args.mkString(", ")}))"
    }
  }

  def disambiguate(x: EirExpressionNode): EirNode = {
    x.disambiguation.getOrElse(x match {
      case x: EirResolvable[_] => Find.uniqueResolution(x)
      case x => x
    })
  }

  override def visitFunctionCall(ctx: UnparseContext, x: EirFunctionCall): String = {
    val disambiguated = disambiguate(x.target)
    val isSystem = disambiguated.annotations.exists(_.name == "system")
    val args: List[String] = visitArguments(ctx)(Some(disambiguated), x.args)
    if (isSystem) visitSystemCall(ctx, x.target, disambiguated, args)
    else s"(${visit(ctx, x.target)}${visitSpecialization(ctx, x)}(${args mkString ", "}))"
  }

  override def visitForLoop(ctx: UnparseContext, x: EirForLoop): String = {
    x.header match {
      case EirCStyleHeader(d, t, i) => {
        val decl = visit(ctx, d).headOption.getOrElse(";")
        val test = visit(ctx, t).headOption.getOrElse("")
        val incr = visit(ctx, i).headOption.getOrElse("")
        s"for ($decl $test; $incr) " + visit(ctx, x.body)
      }
      case _ => ???
    }
  }

  override def visitLiteral(ctx: UnparseContext, x: EirLiteral): String = {
    if (x.`type` == EirLiteralTypes.String) s"std::string(${x.value})"
    else x.value
  }

  override def visitSymbol[A <: EirNamedNode](ctx: UnparseContext, x: EirSymbol[A]): String = {
    nameFor(ctx, Find.uniqueResolution(x))
  }

  override def visitDeclaration(ctx: UnparseContext, x: EirDeclaration): String = {
    s"${typeFor(ctx, x.declaredType)} ${nameFor(ctx, x)}" + {
      x.initialValue.map(x => s" = ${visit(ctx, x)}").getOrElse("")
    } + ";"
  }

  override def visitTemplateArgument(ctx: UnparseContext, x: EirTemplateArgument): String = {
    s"typename ${nameFor(ctx, x)}"
  }

  def visitInherits(ctx: UnparseContext, x: EirClassLike): String = {
    val parents = (x.extendsThis ++ x.implementsThese).map(visit(ctx, _))
    if (parents.nonEmpty) {
      ": " + parents.map("public " + _).mkString(", ") + x.extendsThis.map(_ => "").getOrElse(", public PUP::able")
    } else if (!x.isInstanceOf[EirTrait]) ": public PUP::able"
    else ""
  }

  override def visitClassLike(ctx: UnparseContext, x: EirClassLike): String = {
    // if (x.annotations.exists(_.name == "system")) "" else
    visitTemplateArgs(ctx, x.templateArgs) + s"${n}struct ${nameFor(ctx, x)}" + visitInherits(ctx, x) + {
      ctx.numTabs += 1
      val res =if (x.isInstanceOf[EirTrait]) {
        s" {$n${ctx.t}static ${nameFor(ctx, x)}* fromPuppable(PUP::able *p);"
      } else {
        // TODO PUPable_decl_base_template
        s" {$n${ctx.t}PUPable_decl_inside(${nameFor(ctx, x)});" +
          s"$n${ctx.t}${nameFor(ctx, x)}(CkMigrateMessage *m) : PUP::able(m) { }" +
          x.implementsThese.map(x => s"$n${ctx.t}friend class ${nameFor(ctx, x)};").mkString("")
      }
      ctx.numTabs -= 1
      res
    } + visitChildren(ctx, x.members).tail + s";"
  }

  override def visitMember(ctx: UnparseContext, x: EirMember): String = {
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: UnparseContext, args : List[EirTemplateArgument]): String = {
    if (args.isEmpty) ""
    else s"template<${args.map(visit(ctx, _)) mkString ", "}>$n${ctx.t}"
  }

  def generateAssignments(ctx: UnparseContext, x: EirFunction): String = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx.numTabs += 1
    val res = x.functionArgs.filter(_.isSelfAssigning).map(x => {
      n + ctx.t + "this->" + x.name + "=" + x.name + ";"
    }).mkString("")
    ctx.numTabs -= 1
    res
  }

  override def visitFunction(ctx: UnparseContext, x: EirFunction): String = {
    if (visited.contains(x)) return ""
    else if (x.parent.exists(_.annotations.exists(_.name == "system"))) return error(ctx, x)
    visited +:= x
    val virtual =
      Option.when(x.parent.to[EirMember].exists(_.isVirtual))("virtual ").getOrElse("")
    val isEntry = x.scope.exists(_.isInstanceOf[EirProxy])
    val body = x.body.map(visit(ctx, _)).map(s => {
      s"{" + generateAssignments(ctx, x) + s.tail
    }).getOrElse({
      if (virtual.nonEmpty) "= 0;"
      else ";"
    })
    val cons = x.parent.exists({
      case m: EirMember => m.isConstructor
      case _ => false
    })
    val args = {
      val tmp = x.functionArgs.map(visitFunctionArgument(ctx, _, isEntry))
      // NOTE kludgy solution to detect if we need to drop selfProxy
      if (cons && x.parent.flatMap(_.parent).to[EirProxy].isDefined) {
        if (tmp.isEmpty && !globals.strict) tmp else tmp.tail
      } else {
        tmp
      }
    }
    val retTy = if (cons) "" else { visit(ctx, x.returnType) + " " }
    val static = x.parent.collect({
      case m : EirMember if m.isStatic => "static "
    }).getOrElse("")
//    val const = x.parent.collect({
//      case m : EirMember if m.isConst => " const"
//    }).getOrElse("")
    val over = x.parent.collect({
      case m : EirMember if m.isOverride => " override"
    }).getOrElse("")
    visitTemplateArgs(ctx, x.templateArgs) +
    s"$static$virtual$retTy${nameFor(ctx, x)}(${args mkString ", "})$over $body"
  }

  override def visitAnnotations(ctx: UnparseContext, annotations: Iterable[EirAnnotation]): String = {
    if (annotations.nonEmpty) s"$n${ctx.t}/* " + super.visitAnnotations(ctx, annotations) + "*/ "
    else ""
  }

  override def visitBinaryExpression(ctx: UnparseContext, x: EirBinaryExpression): String = {
    s"(${visit(ctx, x.lhs)} ${x.op} ${visit(ctx, x.rhs)})"
  }

  override def nameFor(ctx: UnparseContext, x : EirNode): String = {
    val alias =
      x.annotation("system").flatMap(_("alias")).map(_.stripped)
    x match {
      case _ if alias.isDefined => alias.get
//      case n : EirNamedNode if n.name == "unit" => "void"
//      case n : EirNamedNode if n.name == "selfProxy" => "thisProxy"
      case p : EirProxy => {
        val prefix =
          if (p.isElement) "CProxyElement_" else "CProxy_"
        prefix + p.baseName
      }
      case _ => super.nameFor(ctx, x)
    }
  }

  override def visitFunctionArgument(ctx: UnparseContext, x: EirFunctionArgument): String =
    visitFunctionArgument(ctx, x, isProxyArgument = false)

  def visitFunctionArgument(ctx: UnparseContext, x: EirFunctionArgument, isProxyArgument: Boolean): String = {
    val declTy = Find.uniqueResolution(x.declaredType)
    val argTy = typeFor(ctx, declTy, isProxyArgument)
      // { if (x.isFinal) s"const " else "" } + visit(ctx, x.declaredType) + "&"
    s"$argTy ${nameFor(ctx, x)}${if (isProxyArgument && declTy.isPointer) "_" else ""}"
  }

  override def visitTupleExpression(ctx: UnparseContext, x: EirTupleExpression): String = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    s"std::$func(${visit(ctx, x) mkString ", "})"
  }

  override def visitLambdaExpression(ctx: UnparseContext, x: EirLambdaExpression): String = {
    val retTy = handleOption(ctx, x.foundType).getOrElse("")
    s"[=] (${visit(ctx, x.args) mkString ", "}) -> $retTy ${visit(ctx, x.body)}"
  }

  override def visitNew(ctx: UnparseContext, x: EirNew): String = {
    val args: List[String] = visitArguments(ctx)(x.disambiguation, x.args)
    val objTy: EirType = Find.uniqueResolution(x.target)
    objTy match {
      case _: EirProxyType | _: EirProxy =>
        nameFor(ctx, objTy) + s"::ckNew(${args mkString ", "})"
      case t: EirType if t.isPointer => s"std::make_shared<${nameFor(ctx, t)}>(${args mkString ", "})"
      case _ => super.visitNew(ctx, x)
    }
  }

  override def visitProxy(ctx: UnparseContext, x: EirProxy): String = {
    val ns = x.namespaces.toList
    ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {").mkString("") + n + {
      if (x.isAbstract) visitAbstractProxy(ctx, x)
      else visitConcreteProxy(ctx, x)
    } + n + ns.map(_ => "}").mkString("")
  }

  def visitAbstractPup(ctx: UnparseContext, numImpls: Int): String = {
    ctx.numTabs += 1
    val body =
      s"$n${ctx.t}p | handle;" +
      (0 until numImpls).map(x =>
        s"$n${ctx.t}p | p$x;"
      ).mkString("")
    ctx.numTabs -= 1
    s"$n${ctx.t}void pup(PUP::er &p) {" + body + s"$n${ctx.t}}"
  }

  def visitAbstractEntry(ctx: UnparseContext, f: EirFunction, numImpls: Int): String = {
    val args = f.functionArgs
    val name = nameFor(ctx, f)
    val fArgs = args.map(visit(ctx, _)).mkString(", ")
    val nArgs = args.map(nameFor(ctx, _)).mkString(", ")
    // TODO support non-void returns
    s"$n${ctx.t}void $name($fArgs) {" + {
      s"$n${ctx.t}switch (handle) {" +
        (0 until numImpls).map(x => s"$n${ctx.t}case $x: { p$x.$name($nArgs); break; }").mkString("") +
        s"$n${ctx.t}default: { CkAssert(-1); break; }" +
      s"$n${ctx.t}}"
    } + s"$n${ctx.t}}"
  }

  def visitAbstractProxy(ctx: UnparseContext, x: EirProxy): String = {
    val name = nameFor(ctx, x)
    val impls = x.derived.map(nameFor(ctx, _))
    s"struct $name {" + {
      ctx.numTabs += 1
      val res = s"$n${ctx.t}int handle;" +
        impls.zipWithIndex.map({
          case (derived, idx) => {
            s"$n${ctx.t}$derived p$idx;" +
              s"$n${ctx.t}$name($derived x) : handle($idx), p$idx(x) {};"
          }
        }).mkString("") + {
        s"$n${ctx.t}$name() : handle(-1) {};"
        } + visitAbstractPup(ctx, impls.length) +
        x.members.map(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member), impls.length)).mkString("")
      ctx.numTabs -= 1
      res
    } + s"$n${ctx.t}};"
  }

  def visitConcreteProxy(ctx: UnparseContext, x: EirProxy): String = {
    val base = nameFor(ctx, x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    // TODO add destructor and pupper
    s"struct $name: public CBase_$name {" + {
      ctx.numTabs += 1
      val res = x.membersToGen
        .map(x => s"$n${ctx.t}${visitProxyMember(ctx, x)}").mkString("") +
        s"$n${ctx.t}$base *impl_;"
      ctx.numTabs -= 1
      res
    } + s"$n${ctx.t}};"
  }

  def makeArgsVector(ctx: UnparseContext, name: String): String = {
    s"$n${ctx.t}std::vector<std::string> $name(msg->argc);$n" +
    s"${ctx.t}std::transform(msg->argv, msg->argv + msg->argc, $name.begin(),$n" +
    s"${ctx.t}${ctx.t}[](const char* x) -> std::string { return std::string(x); });$n"
  }

  def makeSmartPointer(ctx: UnparseContext)(x: EirFunctionArgument): String = {
    val ty: EirType = Find.uniqueResolution(x.declaredType)
    val asStr = typeFor(ctx, ty)
    if (ty.isPointer) {
      val cast = ty match {
        case _: EirTrait => s"${nameFor(ctx, ty)}::fromPuppable(${x.name}_)"
        case _ => s"(${nameFor(ctx, ty)}*)${x.name}_"
      }
      s"$n${ctx.t}$asStr ${x.name}($cast);"
    }
    else ""
  }

  def visitProxyMember(ctx: UnparseContext, x: EirMember): String = {
    val proxy = x.parent.to[EirProxy]
    val isConstructor = x.isConstructor
    val isCollective = proxy.exists(_.collective.isDefined)
    val index = Option.when(isCollective)("[thisIndex]").getOrElse("")
    val base = proxy.map(x => nameFor(ctx, x.base)).getOrElse("")
    val f = assertValid[EirFunction](x.member)
    val isMain = proxy.exists(_.isMain)
    val isSingleton = proxy.exists(_.singleton)
    val vf = if (isConstructor) {
      // TODO this is hacky, better means of name fetching is necessary
      val name = s"${base}_${proxy.flatMap(_.collective).map(x => s"${x}_").getOrElse("")}"
      if (isMain) {
        name + "(CkArgMsg* msg) "
      } else {
        visit(ctx, f).init.replaceFirst(base, name)
      }
    } else visit(ctx, f).init
    vf + s"{" + {
      val dropCount = if (isConstructor) 1 else 0
      val args = f.functionArgs.drop(dropCount)
      ctx.numTabs += 1
      val res = {
        Option.when(isConstructor && isMain)(
          args.headOption.map(x => makeArgsVector(ctx, x.name))).flatten.getOrElse("")
      } + args.map(makeSmartPointer(ctx)).mkString("") + s"$n${ctx.t}" + {
        if (isConstructor) s"this->impl_ = new $base(${(List(s"thisProxy$index") ++ args.map(nameFor(ctx, _))).mkString(", ")});"
        // TODO support non-void returns
        else s"this->impl_->${nameFor(ctx, f)}(${f.functionArgs.map(nameFor(ctx, _)).mkString(", ")});"
      }
      ctx.numTabs -= 1
      res
    } +s"$n${ctx.t}}$n"
  }

  override def visitMatch(ctx: UnparseContext, x: EirMatch): String = {
    if (x.foundType.contains(globals.typeFor(EirLiteralTypes.Unit))) {
      ???
    } else if (!x.expression.foundType.exists(_.isPointer)) {
      Errors.cannotCast(x, x.expression.foundType.get, globals.objectType)
    } else {
      val argTy = typeFor(ctx, x.expression.foundType.get)
      val retTy = typeFor(ctx, x.foundType.get)
      s"([&]($argTy tmp) -> $retTy {" + {
        ctx.numTabs += 1
        val res = x.cases.map(visitMatchCase(ctx, _, isUnit = false)).mkString("") + s"$n${ctx.t}" +
          "throw \"match not found\";"
        ctx.numTabs -= 1
        res
      } + s"$n${ctx.t}})(${visit(ctx, x.expression)})"
    }
  }

  def visitMatchCase(ctx: UnparseContext, x: EirMatchCase, isUnit: Boolean): String = {
    val declaration = x.declaration match {
      case Some(d : EirDeclaration) => {
        val ty = nameFor(ctx, d.declaredType)
        val name = nameFor(ctx, d)
        visit(ctx, d).init + s" = std::dynamic_pointer_cast<$ty>(tmp); if ($name)"
      }
      case _ => x.condition.map(y => " if (" + visit(ctx, y) + ")").getOrElse("")
    }
    s"$n${ctx.t}{$declaration ${visit(ctx, x.body)}}"
//    val declaration = x._declaration.map({
//      case (name, ty) => name + ": " + visit(ctx, ty)
//    }).getOrElse("_")
//    s"$n${ctx.t}case $declaration $ifCond=> ${visit(ctx, x.body)}"
  }
}
