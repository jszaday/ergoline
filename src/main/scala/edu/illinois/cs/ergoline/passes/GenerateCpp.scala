package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirProxyType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, assertValid}

import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends EirVisitor[CodeGenerationContext, Unit] {
  var visited : List[EirNode] = Nil
  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = this.visit

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

  def systemClasses(): String = {
    """#ifndef __ERGOLINE_OBJECT__
      |#define __ERGOLINE_OBJECT__
      |namespace ergoline {
      |  struct puppable: public PUP::able {
      |    puppable() {}
      |    virtual ~puppable() { }
      |    puppable(CkMigrateMessage *m) : PUP::able(m) { }
      |  };
      |
      |  struct object {
      |    virtual puppable* toPuppable() = 0;
      |  };
      |}
      |#endif /* __ERGOLINE_OBJECT__ */""".stripMargin
  }

  def typeFor(ctx: CodeGenerationContext, x: EirResolvable[EirType], isEntryArgument: Boolean = false): String =
    typeFor(ctx, Find.uniqueResolution[EirType](x), isEntryArgument)

  def typeFor(ctx: CodeGenerationContext, x: EirType, isEntryArgument: Boolean): String = {
    val name = nameFor(ctx, x)
    if (x.isPointer) {
      if (isEntryArgument) {
        x match {
          case c: EirClassLike if c.isAbstract => {
            // TODO fixme
//            if (ctx.lang == "ci") {
//              "CkPointer<ergoline::puppable>"
//            } else {
              "ergoline::puppable*"
//            }
          }
          case _ => name + "*"
        }
      }
      else s"std::shared_ptr<$name>"
    }
    else name
  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirClassLike): String = {
    visitTemplateArgs(ctx, x.templateArgs) + s"struct ${nameFor(ctx, x)};"
  }

//  override def visitNamespace(ctx: CodeGenerationContext, node: EirNamespace): Unit = {
//    val parted = node.children.partition(_.isInstanceOf[EirClassLike])
//    node.children = parted._1.map(_.asInstanceOf[EirClassLike]).sorted ++ parted._2
//    super.visitNamespace(ctx, node)
//  }

  // TODO this will need to support templates
  def makeFromPuppable(ctx: CodeGenerationContext, t: EirTrait): Unit = {
    def getDerived(t: EirTrait): Seq[EirClassLike] = {
      t.derived.partition(_.isInstanceOf[EirTrait]) match {
        case (traits, classes) => traits.map(_.asInstanceOf[EirTrait]).flatMap(getDerived) ++ classes
      }
    }
    val name = nameFor(ctx, t)
    val children = getDerived(t).toSet.map((c: EirClassLike) => {
      val names =
        if (c.templateArgs.isEmpty) List(nameFor(ctx, c))
        else Processes.checked(c).map(x => templatedNameFor(ctx, c, Some(x)))
      names.map(otherName =>
        s"if (p->get_PUP_ID() == $otherName::my_PUP_ID) return ($otherName*) p;").mkString(n)
    }).toList :+ s"return nullptr;$n"
    ctx << s"$name* $name::fromPuppable(ergoline::puppable *p)" << "{" <<
      children.head << children.tail.map("else " + _) << "}"
  }

//  def forwardDefn(ctx: CodeGenerationContext, x: EirClassLike): String = {
//    s"${n}struct ${nameFor(ctx, x)}" + visitInherits(ctx, x) + "{" + {
//      ctx.numTabs += 1
//      val res = ""
//      ctx.numTabs -= 1
//      res
//    } + s"$n};"
//  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirProxy): String = {
    val ns = x.namespaces.toList
    (ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {") ++ {
      Seq(s"struct ${nameFor(ctx, x)};")
    } ++ ns.map(_ => "}")).mkString(n)
  }

  override def error(ctx: CodeGenerationContext, node : EirNode): Unit = ()

//  override def visitArrayReference(ctx: CodeGenerationContext, x: EirArrayReference): Unit = ???

  override def visitFieldAccessor(ctx: CodeGenerationContext, x: EirFieldAccessor): Unit = {
    // TODO handle self applications :3
    val targetTy: EirType = x.target.foundType.getOrElse(Errors.missingType(x.target))
    ctx << x.target << (if (targetTy.isPointer) "->" else ".") << x.field
  }

  override def visitLambdaType(ctx: CodeGenerationContext, x: types.EirLambdaType): Unit = "auto"

  override def visitProxyType(ctx: CodeGenerationContext, x: types.EirProxyType): Unit =
    ctx << nameFor(ctx, Find.uniqueResolution(x))

  override def visitImport(ctx: CodeGenerationContext, x: EirImport): Unit = ()

//  def handleOption(ctx: CodeGenerationContext, x : Option[EirNode]): Option[String] = {
//    x.map({
//      case n : EirNamedNode => nameFor(ctx, n)
//      case n => visit(ctx, n)
//    })
//  }

  def isEntryArgument(f: EirFunctionArgument): Boolean = {
    f.parent.flatMap(_.parent).flatMap(_.parent).exists(_.isInstanceOf[EirProxy])
  }

  def castToPuppable(ctx: CodeGenerationContext)(ptrType: Option[String], expr: EirExpressionNode): String = {
    val puppable = "ergoline::puppable"
    s"CkPointer<${ptrType.getOrElse(puppable)}>(${visit(ctx, expr)}${ptrType.map(_ => ".get").getOrElse("->toPuppable")}())"
  }

  def visitCallArgument(ctx: CodeGenerationContext)(t: (EirExpressionNode, EirFunctionArgument)): String = {
    val theirs = t._2.declaredType.resolve().headOption
    (t._1.foundType, theirs) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if a.isDescendantOf(b) =>
        s"${nameFor(ctx, b)}(${visit(ctx, t._1)})"
      case (Some(tty: EirTemplatedType), Some(_)) if isEntryArgument(t._2) => {
        val base = assertValid[EirClassLike](Find.uniqueResolution(tty.base))
        base.extendsThis match {
          case None => castToPuppable(ctx)(None, t._1)
          case _ => ???
        }
      }
      case (Some(c: EirClassLike), Some(_)) if c.isPointer && isEntryArgument(t._2) =>
        castToPuppable(ctx)(c.extendsThis.map(nameFor(ctx, _)), t._1)
      case _ => "" // TODO visit(ctx, t._1)
    }
  }

  def visitArguments(ctx: CodeGenerationContext)(disambiguation: Option[EirNode], args: List[EirExpressionNode]): List[String] = {
    val theirs: List[EirFunctionArgument] =
      disambiguation match {
        case Some(m@EirMember(_, f: EirFunction, _)) =>
          if (m.isStatic) f.functionArgs
          else f.functionArgs.drop(if (m.isConstructor && m.isEntry) 1 else 0)
        case Some(f: EirFunction) => f.functionArgs
        case _ => Nil
      }
    if (theirs.length == args.length) args.zip(theirs).map(visitCallArgument(ctx))
    else List("") // TODO args.map(visit(ctx, _))
  }

  def visitSystemCall(ctx: CodeGenerationContext, target: EirExpressionNode, disambiguated: EirNode, args: List[String]): String = {
//    val base = target match {
//      case f: EirFieldAccessor => f.target
//      case _ => target
//    }
//    val proxy = disambiguated.parent.to[EirProxy]
//    val system = disambiguated.annotation("system").get
//    val name = system("alias").map(_.stripped).getOrElse(nameFor(ctx, disambiguated))
//    val static = system("static").exists(_.value.toBoolean)
//    val cast = system("cast").exists(_.value.toBoolean)
//    disambiguated.asInstanceOf[EirNamedNode] match {
//      case _ : EirMember if proxy.isDefined =>
//        name match {
//          case "index" =>
//            proxy.flatMap(_.collective) match {
//              case Some(ProxyManager.arrayPtn(dim)) =>
//                val idx = s"${visit(ctx, base)}.ckGetIndex().data()"
//                val tup = s"tuple<${List.fill(dim.toInt)("int") mkString ", "}>"
//                // TODO cast to tuple
//                if (dim == "1") s"($idx[0])" else {
//                  s"([&](int *idx) -> std::$tup { return std::make_$tup(${
//                    (0 until dim.toInt).indices.map("std::forward<int>(idx[" + _ + "])") mkString ", "});})(const_cast<int*>($idx))"
//                }
//              case Some("nodegroup" | "group") => s"(${visit(ctx, base)}.ckGetGroupPe())"
//              case _ => error(ctx, target, "no generation method defined")
//            }
//          case "parent" => s"(CProxy_${proxy.get.baseName}(${visit(ctx, base)}.ckGetArrayID()))"
//          case _ => error(ctx, target, "no generation method defined")
//        }
//      case _ : EirMember if static => s"$name(${(visit(ctx, base) +: args).mkString(", ")})"
//      case EirMember(_, f: EirFunction, _) if cast =>
//        s"((${typeFor(ctx, f.returnType)})${visit(ctx, base)})"
//      case _ : EirMember => s"${visit(ctx, base)}.$name(${args.mkString(", ")})"
//      case f : EirFunction if f.name == "println" => "CkPrintf(\"%s\\n\", " + s"${args.map(x => x + ".c_str()").mkString(", ")})"
//      case _ => s"($name(${args.mkString(", ")}))"
//    }
    ""
  }

  def disambiguate(x: EirExpressionNode): EirNode = {
    x.disambiguation.getOrElse(x match {
      case x: EirResolvable[_] => Find.uniqueResolution(x)
      case x => x
    })
  }

  def visitSpecialization(ctx: CodeGenerationContext, s: EirSpecialization): Unit = {
//    if (s.specialization.nonEmpty) ctx
//    else ctx
  }

  override def visitFunctionCall(ctx: CodeGenerationContext, x: EirFunctionCall): Unit = {
    val disambiguated = disambiguate(x.target)
    val isSystem = disambiguated.annotations.exists(_.name == "system")
    val args: List[String] = visitArguments(ctx)(Some(disambiguated), x.args)
    if (isSystem) ctx << visitSystemCall(ctx, x.target, disambiguated, args)
    else ctx << x.target << visitSpecialization(ctx, x) << "(" << (args, ", ") << ")"
  }

  override def visitForLoop(ctx: CodeGenerationContext, x: EirForLoop): Unit = {
    x.header match {
      case EirCStyleHeader(declaration, test, increment) => {
        ctx << s"for (" << (declaration, ";") << test << ";" << increment << ")" << x.body
      }
      case _ => ???
    }
  }

  override def visitLiteral(ctx: CodeGenerationContext, x: EirLiteral): Unit = {
    if (x.`type` == EirLiteralTypes.String) ctx << s"std::string(${x.value})"
    else ctx << x.value
  }

  override def visitSymbol[A <: EirNamedNode](ctx: CodeGenerationContext, x: EirSymbol[A]): Unit = {
    ctx << nameFor(ctx, Find.uniqueResolution(x))
  }

  override def visitDeclaration(ctx: CodeGenerationContext, x: EirDeclaration): Unit = {
    ctx << s"${typeFor(ctx, x.declaredType)} ${nameFor(ctx, x)}" << x.initialValue.map(_ => " = ") << x.initialValue << ";"
  }

  override def visitTemplateArgument(ctx: CodeGenerationContext, x: EirTemplateArgument): Unit = {
    ctx << s"typename ${nameFor(ctx, x)}"
  }

  def visitInherits(ctx: CodeGenerationContext, x: EirClassLike): String = {
    val parents = (x.extendsThis ++ x.implementsThese).map(nameFor(ctx, _))
    if (x.isInstanceOf[EirTrait]) {
      if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(", ") else ": public ergoline::object"
    } else {
      if (parents.isEmpty) ": public ergoline::puppable, public ergoline::object"
      else ": " + parents.map("public " + _).mkString(", ") + x.extendsThis.map(_ => "").getOrElse(", public ergoline::puppable")
    }
  }

  def templatedNameFor(ctx: CodeGenerationContext, x: EirSpecializable, specialization: Option[EirSpecialization] = None): String = {
    nameFor(ctx, x) + (if (x.templateArgs.isEmpty) "" else specialization match {
      case Some(x) => s"<${x.specialization.map(typeFor(ctx, _)).mkString(", ")}>"
      case None => s"<${x.templateArgs.map(nameFor(ctx, _)).mkString(", ")}>"
    })
  }

  def puppingParents(x: EirClassLike): List[EirType] = {
    // TODO enable stateful traits?
    // TODO be templating aware?
    (x.extendsThis ++ x.implementsThese).map(Find.uniqueResolution[EirType])
      .filterNot(_.isInstanceOf[EirTrait]).toList
  }

  def makePupper(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    // TODO check to ensure user does not override
    ctx << s"virtual void pup(PUP::er &p) override" << "{" << {
      val parents = puppingParents(x) match {
        case Nil => List(s"PUP::able::pup(p);")
        case _ => ???
      }
      val values = x.members.collect({
        case m@EirMember(_, d: EirDeclaration, _) if m.annotation("transient").isEmpty => d
      }).map(d => s"p | ${nameFor(ctx, d)};")
      (parents ++ values)
    } << s"}"
  }

  def visitClassLike(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
   if (x.annotations.exists(_.name == "system")) return ""
   ctx << visitTemplateArgs(ctx, x.templateArgs) << s"struct ${nameFor(ctx, x)}" << visitInherits(ctx, x) << "{" << {
      if (x.isInstanceOf[EirTrait]) {
        List(s"static ${templatedNameFor(ctx, x)}* fromPuppable(ergoline::puppable *p);")
      } else {
        makePupper(ctx, x)
        // TODO PUPable_decl_base_template
        List(if (x.templateArgs.isEmpty) s"PUPable_decl_inside(${nameFor(ctx, x)});"
             else s"PUPable_decl_inside_template(${templatedNameFor(ctx, x)});",
            s"${nameFor(ctx, x)}(CkMigrateMessage *m) : ergoline::puppable(m) { }",
            "virtual ergoline::puppable* toPuppable() override { return this; }") ++
        Find.traits(x).map(x => s"friend class ${nameFor(ctx, x)};")
      }
    } << x.members << s"};"
  }

  override def visitMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: CodeGenerationContext, args : List[EirTemplateArgument]): Unit = {
    if (args.nonEmpty) ctx << s"template<" << (args, ", ") << ">"
  }

  def generateAssignments(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx << x.functionArgs.filter(_.isSelfAssigning).map(x => {
      "this->" + x.name + "=" + x.name + ";"
    })
  }

  override def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    val virtual =
      Option.when(x.parent.to[EirMember].exists(_.isVirtual))("virtual")
    ctx << virtual << typeFor(ctx, x.returnType) << nameFor(ctx, x) << "(" << (x.functionArgs, ", ") << ")" <<
      (x.body, if (virtual.isDefined) " = 0;" else ";")
//    if (visited.contains(x)) return ""
//    else if (x.annotation("system").isDefined ||
//      x.parent.exists(_.annotations.exists(_.name == "system"))) return ""
//    visited +:= x
//    val virtual =
//      Option.when(x.parent.to[EirMember].exists(_.isVirtual))("virtual ").getOrElse("")
//    val isEntry = x.scope.exists(_.isInstanceOf[EirProxy])
//    val body = x.body.map(visit(ctx, _)).map(s => {
//      s"{" + generateAssignments(ctx, x) + s.tail
//    }).map(" " + _).getOrElse({
//      if (virtual.nonEmpty) " = 0;"
//      else ";"
//    })
//    val cons = x.parent.exists({
//      case m: EirMember => m.isConstructor
//      case _ => false
//    })
//    val args = {
//      val tmp = x.functionArgs.map(visitFunctionArgument(ctx, _, isEntry))
//      // NOTE kludgy solution to detect if we need to drop selfProxy
//      if (cons && x.parent.flatMap(_.parent).to[EirProxy].isDefined) {
//        if (tmp.isEmpty && !globals.strict) tmp else tmp.tail
//      } else {
//        tmp
//      }
//    }
//    val retTy = Option.when(cons)("").getOrElse(typeFor(ctx, x.returnType) + " ")
//    val static = x.parent.collect({
//      case m : EirMember if m.isStatic => "static "
//    }).getOrElse("")
////    val const = x.parent.collect({
////      case m : EirMember if m.isConst => " const"
////    }).getOrElse("")
//    val over = x.parent.collect({
//      case m : EirMember if m.isOverride => " override"
//    }).getOrElse("")
//    visitTemplateArgs(ctx, x.templateArgs) +
//    s"$static$virtual$retTy${nameFor(ctx, x)}(${args mkString ", "})$over$body"
  }

  def visitAnnotations(ctx: CodeGenerationContext, annotations: Iterable[EirAnnotation]): Unit = {
    ctx << "/* " << (annotations.map(_.toString), " ") << " */ "
  }

  override def visitBinaryExpression(ctx: CodeGenerationContext, x: EirBinaryExpression): Unit = {
    ctx << x.lhs << x.op << x.rhs
  }

  def nameFor(ctx: CodeGenerationContext, x : EirNode): String = {
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
      case n: EirNamedNode => n.name
      case r: EirResolvable[_] => nameFor(ctx, Find.uniqueResolution(r))
      case _ => ???
    }
  }

  override def visitFunctionArgument(ctx: CodeGenerationContext, x: EirFunctionArgument): Unit =
    ctx << visitFunctionArgument(ctx, x, isProxyArgument = false)

  def visitFunctionArgument(ctx: CodeGenerationContext, x: EirFunctionArgument, isProxyArgument: Boolean): String = {
    val declTy = Find.uniqueResolution(x.declaredType)
    val argTy = typeFor(ctx, declTy, isProxyArgument)
      // { if (x.isFinal) s"const " else "" } + visit(ctx, x.declaredType) + "&"
    s"$argTy ${nameFor(ctx, x)}${if (isProxyArgument && declTy.isPointer) "_" else ""}"
  }

  override def visitTupleExpression(ctx: CodeGenerationContext, x: EirTupleExpression): Unit = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    ctx << s"std::$func(" << (x.children, ", ") << ")"
  }

  override def visitLambdaExpression(ctx: CodeGenerationContext, x: EirLambdaExpression): Unit = {
    ctx << s"[=] (" << (x.args, ", ") << ") -> " << x.foundType.map(typeFor(ctx, _)) << x.body
  }

  override def visitNew(ctx: CodeGenerationContext, x: EirNew): Unit = {
    val objTy: EirType = Find.uniqueResolution(x.target)
    val moveHeadToLast: Boolean = (objTy match {
      case t: EirProxy => t.collective
      case t: EirProxyType => t.collective
      case _ => None
    }).exists(_.startsWith("array"))
    val args: List[String] = {
      val t: List[String] = visitArguments(ctx)(x.disambiguation, x.args)
      if (moveHeadToLast && t.nonEmpty) t.tail :+ t.head else t
    }
    // TODO fixme
//    objTy match {
//      case _: EirProxyType | _: EirProxy =>
//        nameFor(ctx, objTy) + s"::ckNew(${args mkString ", "})"
//      case t: EirType if t.isPointer => s"std::make_shared<${nameFor(ctx, t)}>(${args mkString ", "})"
//      case _ => visitNew(ctx, x)
//    }
  }

  override def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val ns = x.namespaces.toList
    ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {").mkString("") + n + {
      if (x.isAbstract) visitAbstractProxy(ctx, x)
      else visitConcreteProxy(ctx, x)
    } + n + ns.map(_ => "}").mkString("")
  }

  def visitAbstractPup(ctx: CodeGenerationContext, numImpls: Int): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{" << {
      List(s"p | handle;") ++
        (0 until numImpls).map(x => s"p | p$x;")
    } << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction, numImpls: Int): Unit = {
    val args = f.functionArgs
    val name = nameFor(ctx, f)
    val nArgs = args.map(nameFor(ctx, _)).mkString(", ")
    // TODO support non-void returns
    ctx << s"void $name(" << (args, ", ") << ")" << "{" << {
      List(s"switch (handle)", "{") ++
        (0 until numImpls).map(x => s"case $x: { p$x.$name($nArgs); break; }") ++
      List(s"default: { CkAssert(-1); break; }", "}", "}")
    }
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = nameFor(ctx, x)
    val impls = x.derived.map(nameFor(ctx, _))
    ctx << s"struct $name" << "{" + {
      List(s"int handle;") ++
        impls.zipWithIndex.flatMap({
          case (derived, idx) =>
            List(s"derived p$idx;", s"$name($derived x) : handle($idx), p$idx(x) {};")
        }) ++ List(s"$name() : handle(-1) {};")
    } << visitAbstractPup(ctx, impls.length) << {
      x.members.foreach(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member), impls.length))
    } << "};"
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val base = nameFor(ctx, x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    // TODO add destructor and pupper
    ctx << s"struct $name: public CBase_$name" << "{" << {
      x.membersToGen
        .map(x => visitProxyMember(ctx, x))
    } << s"$base *impl_;" << s"};"
  }

  def makeArgsVector(ctx: CodeGenerationContext, name: String): Unit = {
    ctx << s"std::vector<std::string> $name(msg->argc);" <<
      s"std::transform(msg->argv, msg->argv + msg->argc, $name.begin()," <<
      "[](const char* x) -> std::string { return std::string(x); });"
  }

  def makeSmartPointer(ctx: CodeGenerationContext)(x: EirFunctionArgument): String = {
    val ty: EirType = Find.uniqueResolution(x.declaredType)
    val asStr = typeFor(ctx, ty)
    if (ty.isPointer) {
      val cast = ty match {
        case _: EirTrait => s"${nameFor(ctx, ty)}::fromPuppable(${x.name}_)"
        case _ => s"(${nameFor(ctx, ty)}*)${x.name}_"
      }
      s"$asStr ${x.name}($cast);"
    }
    else ""
  }

  def visitProxyMember(ctx: CodeGenerationContext, x: EirMember): String = {
//    val proxy = x.parent.to[EirProxy]
//    val isConstructor = x.isConstructor
//    val isCollective = proxy.exists(_.collective.isDefined)
//    val index = Option.when(isCollective)("[thisIndex]").getOrElse("")
//    val base = proxy.map(x => nameFor(ctx, x.base)).getOrElse("")
//    val f = assertValid[EirFunction](x.member)
//    val isMain = proxy.exists(_.isMain)
//    val isSingleton = proxy.exists(_.singleton)
//    val vf = if (isConstructor) {
//      // TODO this is hacky, better means of name fetching is necessary
//      val name = s"${base}_${proxy.flatMap(_.collective).map(x => s"${x}_").getOrElse("")}"
//      if (isMain) {
//        name + "(CkArgMsg* msg)"
//      } else {
//        visit(ctx, f).init.replaceFirst(base, name)
//      }
//    } else visit(ctx, f).init
//    vf + s" {" + {
//      val dropCount = if (isConstructor) 1 else 0
//      val args = f.functionArgs.drop(dropCount)
//      ctx.numTabs += 1
//      val res = {
//        Option.when(isConstructor && isMain)(
//          args.headOption.map(x => makeArgsVector(ctx, x.name))).flatten.getOrElse("")
//      } + args.map(makeSmartPointer(ctx)).mkString("") + s"$n${ctx.t}" + {
//        if (isConstructor) s"this->impl_ = new $base(${(List(s"thisProxy$index") ++ args.map(nameFor(ctx, _))).mkString(", ")});"
//        // TODO support non-void returns
//        else s"this->impl_->${nameFor(ctx, f)}(${f.functionArgs.map(nameFor(ctx, _)).mkString(", ")});"
//      }
//      ctx.numTabs -= 1
//      res
//    } +s"$n${ctx.t}}$n"
    ""
  }

  def temporary(ctx: CodeGenerationContext) = "_"

  override def visitMatch(ctx: CodeGenerationContext, x: EirMatch): Unit = {
    val isUnit = x.foundType.contains(globals.typeFor(EirLiteralTypes.Unit))
    val argTy = typeFor(ctx, x.expression.foundType.get)
    val retTy = typeFor(ctx, x.foundType.get)
    ctx << s"([&]($argTy ${temporary(ctx)}) -> $retTy" << "{" << {
      x.cases.map(visitMatchCase(ctx, _, isUnit))
    } << s"})(" << x.expression << ")"
  }

  def visitPatternDecl(ctx: CodeGenerationContext, x: EirPattern, current: String): String = {
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => visitPatternDecl(ctx, p, current)
        case patterns =>
          patterns.zipWithIndex.map({
            case (p, idx) => visitPatternDecl(ctx, p, s"std::get<$idx>($current)")
          }).mkString("")
      }
      case i@EirIdentifierPattern(_, n, t) if n != "_" => ""
      // TODO fixme
//        val ty = Find.uniqueResolution(t)
//        if (ty.isPointer) " " + visit(ctx, i.declarations.head).init + s" = std::dynamic_pointer_cast<${nameFor(ctx, t)}>($current);"
//        else " " + visit(ctx, i.declarations.head).init + s" = $current;"
      case i: EirIdentifierPattern =>
        if (i.name != "_") Errors.missingType(x) else ""
      case _: EirExpressionPattern => ""
      case _ => ???
    }
  }

  def visitPatternCond(ctx: CodeGenerationContext, x: EirPattern, current: String): List[String] = {
//    x match {
//      case EirPatternList(_, ps) => ps match {
//        case p :: Nil => visitPatternCond(ctx, p, current)
//        case patterns =>
//          patterns.zipWithIndex.flatMap {
//            case (p, idx) => visitPatternCond(ctx, p, s"std::get<$idx>($current)")
//          }
//      }
//      case EirIdentifierPattern(_, "_", _) => Nil
//      case EirIdentifierPattern(_, n, t) =>
//        Option.when(Find.uniqueResolution(t).isPointer)(n).toList
//      case e: EirExpressionPattern =>
//        List(s"$current == " << e.expression << ")")
//    }
    Nil
  }

  def visitMatchCase(ctx: CodeGenerationContext, x: EirMatchCase, isUnit: Boolean): String = {
//    val condition = x.condition.map(y => visit(ctx, y))
//    val declarations = visitPatternDecl(ctx, x.patterns, temporary(ctx))
//    val conditions = (condition ++ visitPatternCond(ctx, x.patterns, temporary(ctx))).mkString(" && ")
//    val ifStmt = if (conditions.isEmpty) "" else s" if ($conditions)"
//    val (primary, secondary) = (if (!isUnit) " return" else "", if (isUnit) " return;" else "")
//    s"{$declarations$ifStmt {$primary ${x.body.map(visit(ctx, _)).getOrElse("")};$secondary } }"
    ""
  }

  override def visitTupleType(ctx: CodeGenerationContext, x: types.EirTupleType): Unit = {
    ctx << s"std::tuple<${x.children.map(typeFor(ctx, _)) mkString ", "}>"
  }

  override def visitArrayReference(ctx: CodeGenerationContext, arrayRef: EirArrayReference): Unit = {
    arrayRef.target.foundType match {
      case Some(_ : EirTupleType) => {
        val args = arrayRef.args
        if (args.length != 1 && !args.head.isInstanceOf[EirLiteral]) {
          Errors.invalidTupleIndices(args)
        } else {
          ctx << s"std::get<${visit(ctx, args.head)}>(${visit(ctx, arrayRef.target)})"
        }
      }
      case Some(t) if t.isInstanceOf[EirProxy] => {
        ctx << s"${visit(ctx, arrayRef.target)}(${arrayRef.args.map(visit(ctx, _)) mkString ", "})"
      }
      case Some(t) if !t.isPointer => // TODO fixme
      // super.visitArrayReference(ctx, arrayRef)
      case _ => Errors.missingType(arrayRef.target)
    }
  }

  override def visitMatchCase(ctx: CodeGenerationContext, x: EirMatchCase): Unit = {
    ctx << visitMatchCase(ctx, x, false)
  }

  override def visitPatternList(ctx: CodeGenerationContext, x: EirPatternList): Unit = ???

  override def visitIdentifierPattern(ctx: CodeGenerationContext, x: EirIdentifierPattern): Unit = ???

  override def visitExpressionPattern(ctx: CodeGenerationContext, x: EirExpressionPattern): Unit = ???

  override def visitIfElse(ctx: CodeGenerationContext, x: EirIfElse): Unit = {
    ctx << "if (" << x.test << ")" << x.ifTrue << x.ifFalse.map(_ => "else ") << x.ifFalse
  }

  override def visitSpecializedSymbol(ctx: CodeGenerationContext, x: EirSpecializedSymbol): Unit = ???

  override def visitTernaryOperator(ctx: CodeGenerationContext, x: EirTernaryOperator): Unit = ???

  override def visitTemplatedType(ctx: CodeGenerationContext, x: EirTemplatedType): Unit = ???

  override def visitBlock(ctx: CodeGenerationContext, x: EirBlock): Unit = {
    ctx << "{" << /* (x.children, ";") << */ "}"
  }

  override def visitNamespace(ctx: CodeGenerationContext, x: EirNamespace): Unit = {
    ctx << "namespace " << x.name << "{" << x.children << "}"
  }

  override def visitClass(ctx: CodeGenerationContext, x: EirClass): Unit = visitClassLike(ctx, x)

  override def visitTrait(ctx: CodeGenerationContext, x: EirTrait): Unit = visitClassLike(ctx, x)

  override def visitAnnotation(ctx: CodeGenerationContext, x: EirAnnotation): Unit = {
    ctx << x.toString
  }

  override def visitAssignment(ctx: CodeGenerationContext, x: EirAssignment): Unit = {
    ctx << x.lval << x.op << x.rval << ";"
  }

  override def visitReturn(ctx: CodeGenerationContext, x: EirReturn): Unit = {
    ctx << "return " << x.expression << ";"
  }
}
