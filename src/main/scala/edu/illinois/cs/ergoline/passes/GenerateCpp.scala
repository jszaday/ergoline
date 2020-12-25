package edu.illinois.cs.ergoline.passes

import java.io.File

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

      def isTrait: Boolean = eirType match {
        case x : EirTemplatedType =>
          Find.uniqueResolution[EirType](x.base).isTrait
        case _ : EirTrait => true
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
    val name = nameFor(ctx, t, includeTemplates = true)
    if (t.templateArgs.nonEmpty) visitTemplateArgs(ctx, t.templateArgs)
    ctx << s"inline $name* $name::fromPuppable(ergoline::puppable *p)" << "{" <<
      s"auto q = dynamic_cast<$name*>(p);" <<
      "if (p && !q) CkAbort(\"unable to restore" << name << "!\");" <<
      "return q;" << "}"
  }

  def forwardDecl(ctx: CodeGenerationContext, x: EirProxy): String = {
    val ns = x.namespaces.toList
    (ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {") ++ {
      Seq(s"struct ${nameFor(ctx, x)};")
    } ++ ns.map(_ => "}")).mkString(n)
  }

  override def error(ctx: CodeGenerationContext, node : EirNode): Unit = ()

  override def visitFieldAccessor(ctx: CodeGenerationContext, x: EirFieldAccessor): Unit = {
    // TODO handle self applications :3
    val targetTy: EirType = x.target.foundType.getOrElse(Errors.missingType(x.target))
    ctx << x.target << fieldAccessorFor(targetTy) << x.field
  }

  override def visitLambdaType(ctx: CodeGenerationContext, x: types.EirLambdaType): Unit = {
    ctx << "auto"
  }

  override def visitProxyType(ctx: CodeGenerationContext, x: types.EirProxyType): Unit =
    ctx << nameFor(ctx, Find.uniqueResolution(x))

  override def visitImport(ctx: CodeGenerationContext, x: EirImport): Unit = {
//    if (x.wildcard || x.qualified.length == 1) ctx << s"using namespace ${(if (x.wildcard) x.qualified.init else x.qualified) mkString "::"};"
//    else ctx << s"using ${x.qualified.last} = ${x.qualified mkString "::"};"
  }

  def isEntryArgument(f: EirFunctionArgument): Boolean = {
    f.parent.flatMap(_.parent).flatMap(_.parent).exists(_.isInstanceOf[EirProxy])
  }

  def castToPuppable(ctx: CodeGenerationContext)(ptrType: Option[EirResolvable[EirType]], expr: EirExpressionNode): Unit = {
    val puppable = ptrType match {
      case None => "ergoline::puppable"
      case Some(t) => ctx.nameFor(Find.uniqueResolution(t))
    }
    ctx << s"CkPointer<${ptrType.getOrElse(puppable)}>(" << expr << ptrType.map(_ => ".get").getOrElse("->toPuppable") << "())"
  }

  def visitCallArgument(ctx: CodeGenerationContext)(t: (EirExpressionNode, EirFunctionArgument)): Unit = {
    val theirs = t._2.declaredType.resolve().headOption
    (t._1.foundType, theirs) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if a.isDescendantOf(b) =>
        ctx << s"${nameFor(ctx, b)}(" << t._1 << ")"
      case (Some(tty: EirTemplatedType), Some(_)) if tty.isPointer && isEntryArgument(t._2) =>
        val base = assertValid[EirClassLike](Find.uniqueResolution(tty.base))
        castToPuppable(ctx)(base.extendsThis, t._1)
      case (Some(c: EirClassLike), Some(_)) if c.isPointer && isEntryArgument(t._2) =>
        castToPuppable(ctx)(c.extendsThis, t._1)
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
    val invert = system("invert").exists(_.value.toBoolean)
    val invOp = if (invert) "!" else ""
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
          case "parent" => ctx << s"(CProxy_${proxy.get.baseName}(" << base << {
            proxy.get.collective match {
              case Some("group" | "nodegroup") => ".ckGetGroupID()))"
              case Some(s) if s.startsWith("array") => ".ckGetArrayID()))"
            }
          }
          case _ => error(ctx, target)
        }
      case _ : EirMember if static => ctx << s"$name(" << {
        visitArguments(ctx)(Some(disambiguated), base +: args)
      } << ")"
      case EirMember(_, f: EirFunction, _) if cast =>
        ctx << s"((" << ctx.typeFor(f.returnType) << ")" << base << ")"
      case m : EirMember =>
        if (name == "apply") ctx << base << s"(" << visitArguments(ctx)(Some(disambiguated), args) << ")"
        else {
          val fqnOrDot = if (m.isStatic) "::" else "."
          ctx << invOp << base << s"$fqnOrDot$name(" << visitArguments(ctx)(Some(disambiguated), args) << ")"
        }
      case f : EirFunction if f.name == "println" =>
        ctx << "CkPrintf(\"%s\\n\", " << "(" << {
          visitArguments(ctx)(Some(disambiguated), args)
        } << ")" << ".c_str())"
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
    if (s.specialization.nonEmpty) {
      val types = s.specialization.map(Find.uniqueResolution[EirType])
      ctx << "<" << {
        for (t <- types.init) {
          ctx << ctx.typeFor(t)
          ctx << ","
        }
      } << ctx.typeFor(types.last) << ">"
    }
  }

  override def visitFunctionCall(ctx: CodeGenerationContext, x: EirFunctionCall): Unit = {
    val disambiguated = disambiguate(x.target)
    val isAsync = disambiguated.annotation("async").isDefined
    val isSystem = disambiguated.annotations.exists(_.name == "system")
    if (isSystem) {
      ctx << visitSystemCall(ctx, x.target, disambiguated, x.args)
    } else {
      if (isAsync) {
        val retTy = disambiguated match {
          case EirMember(_, f: EirFunction, _) => Find.uniqueResolution(f.returnType)
          case f: EirFunction => Find.uniqueResolution(f.returnType)
          case _ => Errors.missingType(disambiguated)
        }
        ctx << "(([&](){ " << ctx.typeFor(retTy) << temporary(ctx) << ";"
      }
      ctx << x.target << visitSpecialization(ctx, x) << "(" << {
        if (isAsync) {
          ctx << temporary(ctx)
          if (x.args.nonEmpty) ctx << ","
        }
        visitArguments(ctx)(Some(disambiguated), x.args)
      } << ")"
      if (isAsync) {
        ctx << "; return" << temporary(ctx) << "; })())"
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
        ctx << s"for (" <||< (declaration, ";") << test << ";" << increment << ")" << x.body
      case h: EirForAllHeader => {
        val fieldAccessor = h.expression.foundType.map(fieldAccessorFor).getOrElse(Errors.missingType(h.expression))
        // TODO find a better name than it_
        ctx << "{" << "auto it_ =" << h.expression << ";" << "while (it_" << fieldAccessor << "hasNext()) {"
        if (h.identifiers.length == 1) {
          val ident = h.identifiers.head
          if (ident != "_") {
            ctx << "auto" << ident << "= "
          }
          ctx << "it_" << fieldAccessor << "next();"
        } else {
          ???
        }
        ctx.ignoreNext("{")
        // TODO add declarations
        ctx << x.body << "}"
      }
      case _ => ???
    }
  }

  override def visitWhileLoop(ctx: CodeGenerationContext, x: EirWhileLoop): Unit = {
    ctx << s"while (" << x.condition << ")" << x.body
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

  def isTransient(x: EirClassLike): Boolean = {
    x.parent.exists(_.isInstanceOf[EirMember]) || x.annotation("transient").isDefined
  }

  def visitInherits(ctx: CodeGenerationContext, x: EirClassLike): String = {
    val parents = (x.implementsThese ++ x.extendsThis).map(Find.uniqueResolution[EirType]).map(nameFor(ctx, _))
    if (x.isInstanceOf[EirTrait]) {
      if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(", ") else ": public ergoline::object"
    } else {
      ": " + {
        if (parents.isEmpty) "public ergoline::object" else parents.map("public " + _).mkString(", ")
      } + {
        // NOTE for some reason this has to come last? otherwise it can cause failures? strange...
        if (!isTransient(x) && !x.extendsThis.exists(x => Find.uniqueResolution(x).isInstanceOf[EirClass])) ", public ergoline::puppable" else ""
      } + {
        ", public std::enable_shared_from_this<" + nameFor(ctx, x, includeTemplates = true) +">"
      }
    }
  }

  def puppingParents(x: EirClassLike): List[EirType] = {
    // TODO enable stateful traits?
    // TODO be templating aware?
    (x.extendsThis ++ x.implementsThese).map(Find.uniqueResolution[EirType])
      .filterNot(_.isTrait).toList
  }

  def pupperFor(ctx: CodeGenerationContext, name: String, resolvable: EirResolvable[EirType]): String = {
    val ty = Find.uniqueResolution(resolvable)
    if (ty.isPointer) {
      val typeName = nameFor(ctx, ty)
      s"{ PUP::able* _ = (p.isUnpacking()) ? nullptr : $name->toPuppable(); p(&_); if (p.isUnpacking()) { $name.reset(dynamic_cast<$typeName*>(_)); } };"
    }
    else "p | " + name + ";"
  }

  def makePupper(ctx: CodeGenerationContext, x: EirClassLike): Unit = {
    // TODO check to ensure user does not override
    ctx << s"virtual void pup(PUP::er &p) override" << "{" << {
      val parents = puppingParents(x) match {
        case Nil => List(s"PUP::able::pup(p);")
        case ps => ps.map(nameFor(ctx, _)).map(_ + "::pup(p);")
      }
      val values = x.members.collect({
        case m@EirMember(_, d: EirDeclaration, _) if m.annotation("transient").isEmpty => d
      }).map(d => {
        pupperFor(ctx, nameFor(ctx, d), d.declaredType)
      })
      parents ++ values
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
    if (args.nonEmpty) ctx << s"template<" << (args, ", ") << "> "
  }

  def generateAssignments(ctx: CodeGenerationContext, x: EirFunction): Unit = {
    // TODO generate constructor assignments (i.e. vals with expressions)
    ctx << x.functionArgs.filter(_.isSelfAssigning).map(x => {
      "this->" + x.name + "=" + x.name + ";"
    })
  }

  def visitFunction(ctx: CodeGenerationContext, x: EirFunction, isMember: Boolean): Unit = {
    val member = x.parent.to[EirMember]
    val langCi = ctx.language == "ci"
    if (member.flatMap(_.annotation("system")).orElse(x.annotation("system")).isDefined) {
      return
    }
    val asyncCi = langCi && isMember && member.flatMap(_.annotation("async")).isDefined
    val parent = member.flatMap(_.parent).to[EirClassLike]
    val isConstructor = member.exists(_.isConstructor)
    val overrides = Option.when(isMember && member.exists(_.isOverride))(" override")
    if (!isMember && (parent.exists(_.isAbstract) && x.body.isEmpty)) {
      return
    }
    val name = parent match {
      case Some(p : EirProxy) if langCi && isConstructor => p.baseName
      case Some(classLike) if !isMember => nameFor(ctx, classLike) + "::" + nameFor(ctx, x)
      case _ => nameFor(ctx, x)
    }
    val virtual = Option.when(isMember && !langCi && member.exists(_.isVirtual))("virtual")
    val dropCount = if (langCi && isConstructor) 1 else 0
    ctx << virtual
    // TODO add templates when !isMember
    if (asyncCi) {
      ctx << "void"
    } else {
      Option.when(!isConstructor)(ctx.typeFor(x.returnType))
    }
    val args = x.functionArgs.drop(dropCount)
    ctx << name << "("
    if (asyncCi) {
      ctx << ctx.typeFor(x.returnType)
      if (args.nonEmpty) ctx << ","
    }
    ctx << (args, ", ") << ")" << overrides
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
    val declarations = Option.when(isConstructor)(
      parent.map(_.members).getOrElse(Nil).map(_.member).collect({
        case d: EirDeclaration if d.initialValue.isDefined => d
      })).toIterable.flatten
    ctx << {
      val assignments = x.functionArgs.filter(_.isSelfAssigning)
      if (assignments.nonEmpty || declarations.nonEmpty) {
        ctx << "{"
        ctx.ignoreNext("{")
      }
      assignments.map(arg => {
        val name = nameFor(ctx, arg)
        "this->" + name + "=" + name + ";"
      })
    } << {
      declarations.foreach(d => {
        ctx << "this->" + nameFor(ctx, d) << "=" << d.initialValue << ";"
      })
    } <||< (x.body, ";")
  }

  override def visitFunction(ctx: CodeGenerationContext, x: EirFunction): Unit = visitFunction(ctx, x, isMember = false)

  def visitAnnotations(ctx: CodeGenerationContext, annotations: Iterable[EirAnnotation]): Unit = {
    ctx << "/* " << (annotations.map(_.toString), " ") << " */ "
  }

  override def visitBinaryExpression(ctx: CodeGenerationContext, x: EirBinaryExpression): Unit = {
    ctx << "(" << x.lhs << x.op << x.rhs << ")"
  }

  def qualifiedNameFor(ctx: CodeGenerationContext, usage: EirNode, includeTemplates: Boolean = false)(of: EirNode): String = {
    val ours = Find.parentOf[EirNamespace](usage)
    val theirs = Find.parentOf[EirNamespace](of match {
      case t: EirTemplatedType => Find.uniqueResolution(t.base)
      case n: EirNamedNode => n
      case _ => ???
    })
    val qualifications: Seq[String] = if (ours != theirs) {
      (theirs.get.name +: Find.ancestors(theirs.get).collect{
        case n: EirNamespace => n.name
      }).reverse
    } else Nil
    (qualifications :+ nameFor(ctx, of, includeTemplates)).mkString("::")
  }

  def nameFor(ctx: CodeGenerationContext, x : EirNode, includeTemplates: Boolean = false): String = {
    val alias =
      x.annotation("system").flatMap(_("alias")).map(_.stripped)
    val dealiased = alias.orElse(x match {
      case n: EirNamedNode => Some(n.name)
      case _ => None
    })
    x match {
      case _ if dealiased.contains("self") => {
        val ty = x match {
          case e: EirExpressionNode if e.foundType.isDefined => e.foundType.get
          case d: EirDeclaration => Find.uniqueResolution(d.declaredType)
          case EirMember(_, d: EirDeclaration, _) => Find.uniqueResolution(d.declaredType)
          case _ => Errors.missingType(x)
        }
        "(" + nameFor(ctx, ty, includeTemplates = true) + "::shared_from_this())"
      }
      case x: EirTemplateArgument =>
        ctx.hasSubstitution(x) match {
          case Some(t) => nameFor(ctx, t)
          case None => dealiased.get
        }
      case x: EirTemplatedType => nameFor(ctx, Find.uniqueResolution(x.base)) + "<" + {
        x.args.map(Find.uniqueResolution[EirType]).map(nameFor(ctx, _)).mkString(", ")
      } + ">"
      case x: EirSpecializable with EirNamedNode if x.templateArgs.nonEmpty =>
        val subst = x.templateArgs.map(ctx.hasSubstitution)
        val substDefined = subst.forall(_.isDefined)
        dealiased.get + (if (includeTemplates || substDefined) {
           "<" + {
             if (substDefined) subst.map(x => nameFor(ctx, x.get))
             else x.templateArgs.map(nameFor(ctx, _))
           }.mkString(", ") + ">"
        } else "")
      case p : EirProxy =>
        val prefix =
          if (p.isElement) "CProxyElement_" else "CProxy_"
        prefix + p.baseName
      case _: EirNamedNode => dealiased.get
    }
  }

  override def visitFunctionArgument(ctx: CodeGenerationContext, x: EirFunctionArgument): Unit = {
    val langCi = ctx.language == "ci"
    val declTy = Find.uniqueResolution(x.declaredType)
    if (langCi && declTy.isPointer) ctx << "CkPointer<" + {
      if (declTy.isTrait) "ergoline::puppable"
      else nameFor(ctx, declTy)
    } + ">"
    else ctx << ctx.typeFor(declTy)
    ctx << ctx.nameFor(x)
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
    objTy match {
      case _: EirProxyType | _: EirProxy =>
        ctx << nameFor(ctx, objTy) << s"::ckNew(" << visitArguments(ctx)(x.disambiguation, args) << ")"
      case t: EirType if t.isPointer => ctx << s"std::make_shared<" << qualifiedNameFor(ctx, x)(t) << ">(" << visitArguments(ctx)(x.disambiguation, args) << ")"
      case _ => ctx << "new" << ctx.typeFor(objTy)  << "(" << visitArguments(ctx)(x.disambiguation, args) << ")"
    }
  }

  def temporary(ctx: CodeGenerationContext) = "_"

  override def visitMatch(ctx: CodeGenerationContext, x: EirMatch): Unit = {
    // TODO restore failure to match CmiAbort/throw!
    ctx << s"([&](" << ctx.typeFor(x.expression.foundType.get) << s"${temporary(ctx)}) ->" << ctx.typeFor(x.foundType.get) << "{" << x.cases << {
      val location = Errors.contextualize(x)
      "CkAbort(\"no match found at " + location.substring(location.lastIndexOf(File.separator) + 1) + "\");"
    }<< "})(" << x.expression << ")"
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

  def visitPatternCond(x: EirPattern, current: String, parentType: Option[EirType]): List[String] = {
    x match {
      case EirPatternList(_, ps) => ps match {
        case p :: Nil => visitPatternCond(p, current, parentType)
        case patterns =>
          patterns.zipWithIndex.flatMap {
            case (p, idx) => visitPatternCond(p, s"std::get<$idx>($current)", parentType)
          }
      }
      case EirIdentifierPattern(_, "_", t) => {
        if (parentType.contains(t)) Nil
        else {
          val ctx = new CodeGenerationContext
          List(s"std::dynamic_pointer_cast<${nameFor(ctx, t)}>($current)")
        }
      }
      case EirIdentifierPattern(_, n, t) =>
        Option.when(Find.uniqueResolution(t).isPointer)(n).toList
      case e: EirExpressionPattern =>
        val ctx = new CodeGenerationContext
        (ctx << current << " == " << e.expression).toString.split(n).toList
    }
  }

  override def visitMatchCase(ctx: CodeGenerationContext, x: EirMatchCase): Unit = {
    val parent = x.parent.to[EirMatch]
    val parentType = parent.flatMap(_.foundType)
    val isUnit = parentType.contains(globals.typeFor(EirLiteralTypes.Unit))
    ctx << "{" << visitPatternDecl(x.patterns, temporary(ctx)).split(n)
    val conditions = visitPatternCond(x.patterns, temporary(ctx), parentType).mkString(" && ")
    val needsIf = x.condition.nonEmpty || conditions.nonEmpty
    if (needsIf) ctx << "if(" << x.condition << {
      Option.when(x.condition.isDefined && conditions.nonEmpty)(" && ")
    } << conditions << ")" << "{"
    val (primary, secondary) = (Option.when(!isUnit)(" return"), Option.when(isUnit)("return;"))
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
    arrayRef.target.foundType match {
      case Some(_ : EirTupleType) =>
        val args = arrayRef.args
        if (args.length != 1 && !args.head.isInstanceOf[EirLiteral]) {
          Errors.invalidTupleIndices(args)
        } else {
          ctx << s"std::get<" << args.head << ">(" << arrayRef.target << ")"
        }
      case Some(t : EirProxy) if t.collective.exists(_.startsWith("array")) =>
        ctx << arrayRef.target << "(" << (arrayRef.args, ",") << ")"
      case Some(t) if !t.isPointer =>
        ctx << arrayRef.target
        for (arg <- arrayRef.args) {
          ctx << "[" << arg << "]"
        }
      case _ => Errors.missingType(arrayRef.target)
    }
  }


  override def visitPatternList(ctx: CodeGenerationContext, x: EirPatternList): Unit = ???
  override def visitIdentifierPattern(ctx: CodeGenerationContext, x: EirIdentifierPattern): Unit = ???
  override def visitExpressionPattern(ctx: CodeGenerationContext, x: EirExpressionPattern): Unit = ???
  override def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = ???

  override def visitSpecializedSymbol(ctx: CodeGenerationContext, x: EirSpecializedSymbol): Unit = {
    val base = Find.uniqueResolution(x.symbol)
    ctx << nameFor(ctx, base) << visitSpecialization(ctx, x)
  }

  override def visitIfElse(ctx: CodeGenerationContext, x: EirIfElse): Unit = {
    ctx << "if (" << x.test << ")" << x.ifTrue << x.ifFalse.map(_ => "else ") << x.ifFalse
  }

  override def visitTernaryOperator(ctx: CodeGenerationContext, x: EirTernaryOperator): Unit = {
    ctx << "(" << x.test << "?" << x.ifTrue << ":" << x.ifFalse << ")"
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
}
