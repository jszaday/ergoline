package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirProxyType, EirTemplatedType, EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
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

  def forwardDecl(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    visitTemplateArgs(ctx, x.templateArgs)
    ctx << s"struct ${nameFor(ctx, x)};"
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

  override def visitLambdaType(ctx: CodeGenerationContext, x: types.EirLambdaType): Unit = {
    ctx << "auto"
  }

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

  def castToPuppable(ctx: CodeGenerationContext)(ptrType: Option[String], expr: EirExpressionNode): Unit = {
    val puppable = "ergoline::puppable"
    ctx << s"CkPointer<${ptrType.getOrElse(puppable)}>(" << expr << ptrType.map(_ => ".get").getOrElse("->toPuppable") << "())"
  }

  def visitCallArgument(ctx: CodeGenerationContext)(t: (EirExpressionNode, EirFunctionArgument)): Unit = {
    val theirs = t._2.declaredType.resolve().headOption
    (t._1.foundType, theirs) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if a.isDescendantOf(b) =>
        ctx << s"${nameFor(ctx, b)}(" << t._1 << ")"
      case (Some(tty: EirTemplatedType), Some(_)) if isEntryArgument(t._2) => {
        val base = assertValid[EirClassLike](Find.uniqueResolution(tty.base))
        base.extendsThis match {
          case None => castToPuppable(ctx)(None, t._1)
          case _ => ???
        }
      }
      case (Some(c: EirClassLike), Some(_)) if c.isPointer && isEntryArgument(t._2) =>
        castToPuppable(ctx)(c.extendsThis.map(nameFor(ctx, _)), t._1)
      case _ => ctx << t._1
    }
  }

  def visitArguments(ctx: CodeGenerationContext)(disambiguation: Option[EirNode], args: List[EirExpressionNode]): Unit = {
    val theirs: List[EirFunctionArgument] =
      disambiguation match {
        case Some(m@EirMember(_, f: EirFunction, _)) =>
          if (m.isStatic) f.functionArgs
          else f.functionArgs.drop(if (m.isConstructor && m.isEntry) 1 else 0)
        case Some(f: EirFunction) => f.functionArgs
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
      }

    } else {
      ctx << (args, ",")
    }
  }

  def visitSystemCall(ctx: CodeGenerationContext, target: EirExpressionNode,
                      disambiguated: EirNode, args: List[EirExpressionNode]): Unit = {
    val base = target match {
      case f: EirFieldAccessor => f.target
      case _ => target
    }
    val proxy = disambiguated.parent.to[EirProxy]
    val system = disambiguated.annotation("system").get
    val static = system("static").exists(_.value.toBoolean)
    val cast = system("cast").exists(_.value.toBoolean)
    val name = system("alias").map(_.stripped).getOrElse(nameFor(ctx, disambiguated))
    disambiguated.asInstanceOf[EirNamedNode] match {
      case _ : EirMember if proxy.isDefined =>
        name match {
          case "index" =>
            proxy.flatMap(_.collective) match {
              case Some(ProxyManager.arrayPtn(dim)) =>
                val tup = s"tuple<${List.fill(dim.toInt)("int") mkString ", "}>"
                if (dim != "1") {
                  ctx << s"([&](int *idx) -> std::$tup { return std::make_$tup(${
                    (0 until dim.toInt).indices.map("std::forward<int>(idx[" + _ + "])") mkString ", "
                  });})(const_cast<int*>"
                }
                ctx << "(" << base << ".ckGetIndex().data()" << (if (dim == "1") "[0])" else "))")
              case Some("nodegroup" | "group") => ctx << "(" << base << ".ckGetGroupPe())"
              case _ => error(ctx, target)
            }
          case "parent" => ctx << s"(CProxy_${proxy.get.baseName}(" << base << ".ckGetArrayID()))"
          case _ => error(ctx, target)
        }
      case _ : EirMember if static => ctx << s"$name(" << {
        visitArguments(ctx)(Some(disambiguated), base +: args)
      } << ")"
      case EirMember(_, f: EirFunction, _) if cast =>
        ctx << s"((" << ctx.typeFor(f.returnType) << ")" << base << ")"
      case _ : EirMember => ctx << base << s".$name" << visitArguments(ctx)(Some(disambiguated), args) << ")"
      case f : EirFunction if f.name == "println" => {
        ctx << "CkPrintf(\"%s\\n\", " << "(" << {
          visitArguments(ctx)(Some(disambiguated), args)
        } << ")" << ".c_str())"
      }
      case _ => ctx << s"($name(" << visitArguments(ctx)(Some(disambiguated), args) << "))"
    }
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
    if (isSystem) {
      ctx << visitSystemCall(ctx, x.target, disambiguated, x.args)
    }
    else {
      ctx << x.target << visitSpecialization(ctx, x) << "(" << {
        visitArguments(ctx)(Some(disambiguated), x.args)
      } << ")"
    }
  }

  override def visitForLoop(ctx: CodeGenerationContext, x: EirForLoop): Unit = {
    x.header match {
      case EirCStyleHeader(declaration, test, increment) => {
        ctx << s"for (" <||< (declaration, ";") << test << ";" << increment << ")" << x.body
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
    ctx << ctx.typeFor(x.declaredType) << s"${nameFor(ctx, x)}" << x.initialValue.map(_ => "= ") << x.initialValue << ";"
  }

  override def visitTemplateArgument(ctx: CodeGenerationContext, x: EirTemplateArgument): Unit = {
    ctx << s"typename ${nameFor(ctx, x)}"
  }

  def visitInherits(ctx: CodeGenerationContext, x: EirClassLike): String = {
    val parents = (x.extendsThis ++ x.implementsThese).map(Find.uniqueResolution[EirType]).map(nameFor(ctx, _))
    if (x.isInstanceOf[EirTrait]) {
      if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(", ") else ": public ergoline::object"
    } else {
      if (parents.isEmpty) ": public ergoline::puppable, public ergoline::object"
      else ": " + parents.map("public " + _).mkString(", ") + x.extendsThis.map(_ => "").getOrElse(", public ergoline::puppable")
    }
  }

  def templatedNameFor(ctx: CodeGenerationContext, x: EirSpecializable, specialization: Option[EirSpecialization] = None): String = {
//    nameFor(ctx, x) + (if (x.templateArgs.isEmpty) "" else specialization match {
//      case Some(x) => s"<"{x.specialization.foreach(ctx.typeFor(_)).mkString(", ")}>"
//      case None => s"<${x.templateArgs.map(nameFor(ctx, _)).mkString(", ")}>"
//    })
    ""
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
    if (x.templateArgs.isEmpty) {
      ctx << x.members.filter(_.member.isInstanceOf[EirFunction])
    }
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

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction, isMember: Boolean): Unit = {
    val member = x.parent.to[EirMember]
    if (member.flatMap(_.annotation("system")).orElse(x.annotation("system")).isDefined) {
      return
    }
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val isConstructor = member.exists(_.isConstructor)
    val overrides = Option.when(isMember && member.exists(_.isOverride))(" override")
    if (!isMember && (parent.exists(_.isAbstract) && x.body.isEmpty)) {
      return
    }
    val name = (parent match {
      case Some(classLike) if !isMember => nameFor(ctx, classLike) + "::"
      case _ => ""
    }) + nameFor(ctx, x)
    val virtual = Option.when(isMember && member.exists(_.isVirtual))("virtual")
    ctx << virtual
    // TODO add templates when !isMember
    Option.when(!isConstructor)(ctx.typeFor(x.returnType))
    ctx << name << "(" << (x.functionArgs, ", ") << ")" << overrides
    if (isMember) {
      if (virtual.nonEmpty && x.body.isEmpty) {
        ctx << " = 0;"
        return
      } else if (!parent.exists(_.templateArgs.nonEmpty) && x.templateArgs.isEmpty) {
        ctx << ";"
        return
      }
    } else if (x.body.isEmpty) {
      Errors.missingBody(x)
    }
    ctx << {
      val assignments = x.functionArgs.filter(_.isSelfAssigning)
      if (assignments.nonEmpty) {
        ctx << "{"
        ctx.ignoreNext("{")
      }
      assignments.map(arg => {
        val name = nameFor(ctx, arg)
        "this->" + name + "=" + name + ";"
      })
    } <||< (x.body, ";")
  }

  override def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit = visitFunction(ctx, x, isMember = false)

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
//      case _ => {
//        throw new RuntimeException(s"unsure how to get name of ${x.getClass.getName}")
//      }
    }
  }

  override def visitFunctionArgument(ctx: CodeGenerationContext, x: EirFunctionArgument): Unit = {
    val declTy = Find.uniqueResolution(x.declaredType)
    // { if (x.isFinal) s"const " else "" } + visit(ctx, x.declaredType) + "&"
    ctx << ctx.typeFor(declTy) << ctx.nameFor(x)
  }

  override def visitTupleExpression(ctx: CodeGenerationContext, x: EirTupleExpression): Unit = {
    val func = x.parent match {
      case Some(a : EirAssignment) if a.lval == x => "tie"
      case _ => "make_tuple"
    }
    ctx << s"std::$func(" << (x.children, ", ") << ")"
  }

  override def visitLambdaExpression(ctx: CodeGenerationContext, x: EirLambdaExpression): Unit = {
    ctx << s"[=] (" << (x.args, ", ") << ") -> " << {
      x.foundType.foreach(ctx.typeFor(_))
    } << x.body
  }

  override def visitNew(ctx: CodeGenerationContext, x: EirNew): Unit = {
    val objTy: EirType = Find.uniqueResolution(x.target)
    val moveHeadToLast: Boolean = (objTy match {
      case t: EirProxy => t.collective
      case t: EirProxyType => t.collective
      case _ => None
    }).exists(_.startsWith("array"))
    val args = {
      if (moveHeadToLast && x.args.nonEmpty) x.args.tail :+ x.args.head
      else x.args
    }
    // TODO fixme
    objTy match {
      case _: EirProxyType | _: EirProxy =>
        ctx << nameFor(ctx, objTy) << s"::ckNew(" << visitArguments(ctx)(x.disambiguation, args) << ")"
      case t: EirType if t.isPointer => ctx << s"std::make_shared<" << ctx.nameFor(t) << ">(" << visitArguments(ctx)(x.disambiguation, args) << ")"
      case _ => ctx << "new" << ctx.typeFor(objTy)  << "(" << visitArguments(ctx)(x.disambiguation, args) << ")"
    }
  }

  def temporary(ctx: CodeGenerationContext) = "_"

  override def visitMatch(ctx: CodeGenerationContext, x: EirMatch): Unit = {
//    val isUnit = x.foundType.contains(globals.typeFor(EirLiteralTypes.Unit))
    ctx << s"([&](" << ctx.typeFor(x.expression.foundType.get) << s"${temporary(ctx)}) ->" << ctx.typeFor(x.foundType.get) << "{" << x.cases << "})(" << x.expression << ")"
  }

  def visitPatternDecl(x: EirPattern, current: String): String = {
    val ctx = new CodeGenerationContext
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => ctx << visitPatternDecl(p, current)
        case patterns =>
          ctx << patterns.zipWithIndex.map({
            case (p, idx) => visitPatternDecl(p, s"std::get<$idx>($current)")
          })
      }
      case i@EirIdentifierPattern(_, n, t) if n != "_" =>
      // TODO fixme
        val ty = Find.uniqueResolution(t)
        ctx.ignoreNext(";")
        if (ty.isPointer) ctx << i.declarations.head << s" = std::dynamic_pointer_cast<${nameFor(ctx, t)}>($current);"
        else ctx << i.declarations.head << s" = $current;"
      case i: EirIdentifierPattern =>
        if (i.name != "_") Errors.missingType(x)
      case _: EirExpressionPattern =>
      case _ => ???
    }
    ctx.toString
  }

  def visitPatternCond(x: EirPattern, current: String): List[String] = {
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => visitPatternCond(p, current)
        case patterns =>
          patterns.zipWithIndex.flatMap {
            case (p, idx) => visitPatternCond(p, s"std::get<$idx>($current)")
          }
      }
      case EirIdentifierPattern(_, "_", _) => Nil
      case EirIdentifierPattern(_, n, t) =>
        Option.when(Find.uniqueResolution(t).isPointer)(n).toList
      case e: EirExpressionPattern =>
        val ctx = new CodeGenerationContext
        (ctx << current << " == " << e.expression).toString.split(n).toList
    }
  }

  override def visitMatchCase(ctx: CodeGenerationContext, x: EirMatchCase): Unit = {
    val parent = x.parent.to[EirMatch]
    val isUnit = parent.exists(_.foundType.contains(globals.typeFor(EirLiteralTypes.Unit)))
    ctx << "{" << visitPatternDecl(x.patterns, temporary(ctx)).split(n)
    val conditions = visitPatternCond(x.patterns, temporary(ctx)).mkString(" && ")
    val needsIf = x.condition.nonEmpty || conditions.nonEmpty
    if (needsIf) ctx << "if(" << x.condition << {
      Option.when(x.condition.isDefined && conditions.nonEmpty)(" && ")
    } << conditions << ")" << "{"
    val (primary, secondary) = (Option.when(!isUnit)(" return"), Option.when(isUnit)(" return;"))
    ctx << primary << x.body << secondary << Option.when(needsIf)("}") << "}"
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
    arrayRef.target.foundType match {
      case Some(_ : EirTupleType) => {
        val args = arrayRef.args
        if (args.length != 1 && !args.head.isInstanceOf[EirLiteral]) {
          Errors.invalidTupleIndices(args)
        } else {
          ctx << s"std::get<" << args.head << ">(" << arrayRef.target << ")"
        }
      }
      case Some(t) if t.isInstanceOf[EirProxy] => {
        ctx << arrayRef.target << "(" << (arrayRef.args, ",") << ")"
      }
      case Some(t) if !t.isPointer => // TODO fixme
      // super.visitArrayReference(ctx, arrayRef)
      case _ => Errors.missingType(arrayRef.target)
    }
  }


  override def visitPatternList(ctx: CodeGenerationContext, x: EirPatternList): Unit = ???

  override def visitIdentifierPattern(ctx: CodeGenerationContext, x: EirIdentifierPattern): Unit = ???

  override def visitExpressionPattern(ctx: CodeGenerationContext, x: EirExpressionPattern): Unit = ???

  override def visitIfElse(ctx: CodeGenerationContext, x: EirIfElse): Unit = {
    ctx << "if (" << x.test << ")" << x.ifTrue << x.ifFalse.map(_ => "else ") << x.ifFalse
  }

  override def visitSpecializedSymbol(ctx: CodeGenerationContext, x: EirSpecializedSymbol): Unit = ???

  override def visitTernaryOperator(ctx: CodeGenerationContext, x: EirTernaryOperator): Unit = {
    ctx << x.test << "?" << x.ifTrue << ":" << x.ifFalse
  }

  override def visitTemplatedType(ctx: CodeGenerationContext, x: EirTemplatedType): Unit = {
    val base = Find.uniqueResolution(x.base)
    ctx << ctx.nameFor(base) << "<" << {
      if (x.args.nonEmpty) {
        x.args.init.foreach(t => {
          ctx.typeFor(t)
          ctx << ", "
        })
        ctx.typeFor(x.args.last)
      }
    } << ">"
  }

  override def visitBlock(ctx: CodeGenerationContext, x: EirBlock): Unit = {
    ctx << "{" << {
      for (child <- x.children) {
        ctx << child
        ctx.appendSemi()
      }
    } << "}"
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
    ctx << x.lval << x.op << x.rval
  }

  override def visitReturn(ctx: CodeGenerationContext, x: EirReturn): Unit = {
    ctx << "return" << x.expression
  }

  override def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = ???
}
