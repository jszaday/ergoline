package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirProxyType
import edu.illinois.cs.ergoline.passes.UnparseAst.UnparseContext
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.assertValid

import scala.util.Properties.{lineSeparator => n}

object GenerateCpp extends UnparseAst {
  var visited : List[EirNode] = Nil

  def visit(node: EirNode): String = visit(new UnparseContext, node)

  def forwardDecl(x: EirProxy, ctx: UnparseContext = new UnparseContext): String = {
    val ns = x.namespaces.toList
    (ns.map(ns => s"${n}namespace ${nameFor(ctx, ns)} {") ++ {
      Seq(s"struct ${nameFor(ctx, x)};")
    } ++ ns.map(_ => "}")).mkString(n)
  }

  override def error(ctx: UnparseContext, node : EirNode): String = {
    s"/* skipped $node */"
  }

  override def visitArrayReference(ctx: UnparseContext, x: EirArrayReference): String = ???

  override def visitFieldAccessor(ctx: UnparseContext, x: EirFieldAccessor): String = {
    // TODO handle self applications :3
    s"${visit(ctx, x.target)}.${x.field}"
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

  def visitCallArgument(ctx: UnparseContext)(t: (EirExpressionNode, EirFunctionArgument)): String = {
    val theirs = t._2.declaredType.resolve().headOption
    (t._1.foundType, theirs) match {
      case (Some(a: EirProxy), Some(b: EirProxy)) if a.isDescendantOf(b) => {
        s"${nameFor(ctx, b)}(${visit(ctx, t._1)})"
      }
      case _ => visit(ctx, t._1)
    }
  }

  def visitArguments(ctx: UnparseContext)(disambiguation: Option[EirNode], args: List[EirExpressionNode]): List[String] = {
    val theirs: List[EirFunctionArgument] =
      disambiguation match {
        case Some(m@EirMember(_, f: EirFunction, _)) =>
          if (m.isStatic) f.functionArgs
          else f.functionArgs.drop(if (m.isConstructor && m.isEntry) 2 else 1)
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
    disambiguated.asInstanceOf[EirNamedNode] match {
      case m : EirMember if m.name == "toString" =>
        s"std::to_string(${visit(ctx, base)})"
      case f : EirFunction if f.name == "exit" => s"CkExit(${args.mkString(", ")})"
      case f : EirFunction if f.name == "println" => "CkPrintf(\"%s\\n\", " + s"${args.map(x => x + ".c_str()").mkString(", ")})"
      case _ => error(ctx, target)
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

  override def visitLiteral(ctx: UnparseContext, x: EirLiteral): String = x.value

  override def visitSymbol[A <: EirNamedNode](ctx: UnparseContext, x: EirSymbol[A]): String = {
    try {
      nameFor(ctx, Find.uniqueResolution(x))
    } catch {
      case _: Throwable => x.qualifiedName.mkString("::")
    }
  }

  override def visitDeclaration(ctx: UnparseContext, x: EirDeclaration): String = {
    s"${visit(ctx, x.declaredType)} ${nameFor(ctx, x)}" + {
      x.initialValue.map(x => s" = ${visit(ctx, x)}").getOrElse("")
    } + ";"
  }

  override def visitTemplateArgument(ctx: UnparseContext, x: EirTemplateArgument): String = {
    s"typename ${nameFor(ctx, x)}"
  }

  def visitInherits(ctx: UnparseContext, x: EirClassLike): String = {
    val parents = (x.extendsThis ++ x.implementsThese).map(visit(ctx, _))
    if (parents.nonEmpty) ": " + parents.map("public " + _).mkString(", ") + " "
    else ""
  }

  override def visitClassLike(ctx: UnparseContext, x: EirClassLike): String = {
    // if (x.annotations.exists(_.name == "system")) "" else
    visitTemplateArgs(ctx, x.templateArgs) + s"${n}struct ${nameFor(ctx, x)} " + visitInherits(ctx, x) + visitChildren(ctx, x.members) + s";"
  }

  override def visitMember(ctx: UnparseContext, x: EirMember): String = {
    visit(ctx, x.member)
  }

  def visitTemplateArgs(ctx: UnparseContext, args : List[EirTemplateArgument]): String = {
    if (args.isEmpty) ""
    else s"template<${args.map(visit(ctx, _)) mkString ", "}>$n${ctx.t}"
  }

  def dropSelf(x : EirFunction): List[EirFunctionArgument] = {
    if (x.functionArgs.isEmpty) x.functionArgs
    else x.parent match {
      case Some(_ : EirMember) if x.functionArgs.head.name == "self" =>  x.functionArgs.tail
      case _ => x.functionArgs
    }
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
    visited +:= x
    val virtual =
      Option.when(x.parent.to[EirMember].exists(_.isVirtual))("virtual ").getOrElse("")
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
      val tmp = dropSelf(x).map(visit(ctx, _))
      // NOTE kludgy solution to detect if we need to drop selfProxy
      if (cons && x.parent.flatMap(_.parent).to[EirProxy].isDefined) {
        tmp.tail
      } else {
        tmp
      }
    }
    val retTy = if (cons) "" else { visit(ctx, x.returnType) + " " }
    val static = x.parent.collect({
      case m : EirMember if m.isStatic => "static "
    }).getOrElse("")
    val const = x.parent.collect({
      case m : EirMember if m.isConst => " const"
    }).getOrElse("")
    val over = x.parent.collect({
      case m : EirMember if m.isOverride => " override"
    }).getOrElse("")
    visitTemplateArgs(ctx, x.templateArgs) +
    s"$static$virtual$retTy${nameFor(ctx, x)}(${args mkString ", "})$const$over $body"
  }

  override def visitAnnotations(ctx: UnparseContext, annotations: Iterable[EirAnnotation]): String = {
    if (annotations.nonEmpty) s"$n${ctx.t}/* " + super.visitAnnotations(ctx, annotations) + "*/ "
    else ""
  }

  override def visitBinaryExpression(ctx: UnparseContext, x: EirBinaryExpression): String = {
    s"(${visit(ctx, x.lhs)} ${x.op} ${visit(ctx, x.rhs)})"
  }

  override def nameFor(ctx: UnparseContext, x : EirNode): String = {
    x match {
      case n : EirNamedNode if n.name == "unit" => "void"
//      case n : EirNamedNode if n.name == "selfProxy" => "thisProxy"
      case n : EirNamedNode if n.name == "self" => "this"
      case p : EirProxy => {
        val prefix =
          if (p.isElement) "CProxyElement_" else "CProxy_"
        prefix + p.baseName
      }
      case _ => super.nameFor(ctx, x)
    }
  }

  override def visitFunctionArgument(ctx: UnparseContext, x: EirFunctionArgument): String = {
    val argTy = visit(ctx, x.declaredType)
      // { if (x.isFinal) s"const " else "" } + visit(ctx, x.declaredType) + "&"
    s"$argTy ${nameFor(ctx, x)}"
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
    x.target match {
      case t: EirProxyType =>
        visit(ctx, t) + s"::ckNew(${args mkString ", "})"
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
    val args = dropSelf(f)
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
      val res = x.members.map(x => s"$n${ctx.t}${visitProxyMember(ctx, x)}").mkString("") +
        s"$n${ctx.t}$base *impl_;"
      ctx.numTabs -= 1
      res
    } + s"$n${ctx.t}};"
  }

  def visitProxyMember(ctx: UnparseContext, x: EirMember): String = {
    val proxy = x.parent.to[EirProxy]
    val isConstructor = x.isConstructor
    val isCollective = proxy.exists(_.collective.isDefined)
    val index = Option.when(isCollective)("[thisIndex]").getOrElse("")
    val base = proxy.map(x => nameFor(ctx, x.base)).getOrElse("")
    val f = assertValid[EirFunction](x.member)
    val vf = if (isConstructor) {
      // TODO this is hacky, better means of name fetching is necessary
      val name = s"${base}_${proxy.flatMap(_.collective).map(x => s"${x}_").getOrElse("")}"
      if (proxy.exists(_.isMain)) {
        name + "(CkArgMsg* msg) "
      } else {
        visit(ctx, f).init.replaceFirst(base, name)
      }
    } else visit(ctx, f).init
    vf + s"{$n" + {
      ctx.numTabs += 1
      val res = s"${ctx.t}" + {
        if (isConstructor) s"this->impl_ = new $base(${(List(s"thisProxy$index") ++ f.functionArgs.tail.tail.map(nameFor(ctx, _))).mkString(", ")});"
        // TODO support non-void returns
        else s"this->impl_->${nameFor(ctx, f)}(${f.functionArgs.tail.map(nameFor(ctx, _)).mkString(", ")});"
      }
      ctx.numTabs -= 1
      res
    } +s"$n${ctx.t}}$n"
  }
}
