package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirFunction, EirFunctionArgument, EirLiteral, EirLiteralTypes, EirMember, EirNode, EirTrait}
import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.GenerateCpp.GenCppSyntax.RichEirType
import edu.illinois.cs.ergoline.passes.GenerateCpp.{nameFor, pupperFor, qualifiedNameFor, temporary}
import edu.illinois.cs.ergoline.proxies.EirProxy
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.assertValid
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

object GenerateProxies {

  implicit val visitor: (CodeGenerationContext, EirNode) => Unit = GenerateCpp.visit

  def visitProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val ns = x.namespaces.toList
    ctx << ns.map(ns => s"namespace ${nameFor(ctx, ns)} {") << {
      if (x.isAbstract) visitAbstractProxy(ctx, x)
      else visitConcreteProxy(ctx, x)
    } << ns.map(_ => "}")
  }

  def visitAbstractPup(ctx: CodeGenerationContext, numImpls: Int): Unit = {
    ctx << s"void pup(PUP::er &p)" << "{" << s"p | handle;" << {
      (0 until numImpls).map(x => s"p | p$x;")
    } << s"}"
  }

  def visitAbstractEntry(ctx: CodeGenerationContext, f: EirFunction, numImpls: Int): Unit = {
    val args = f.functionArgs
    val name = nameFor(ctx, f)
    val nArgs = args.map(nameFor(ctx, _)).mkString(", ")
    // TODO support non-void returns?
    ctx << s"void $name(" << (args, ", ") << ")" << "{" << {
      List(s"switch (handle)", "{") ++
        (0 until numImpls).map(x => s"case $x: { p$x.$name($nArgs); break; }") ++
        List("default: { CkAbort(\"abstract proxy unable to find match\"); }", "}", "}")
    }
  }

  def makeHasher(ctx: CodeGenerationContext, numImpls: Int): Unit = {
    ctx << "virtual std::size_t hash() override" << "{"
    ctx << "ergoline::hasher _;"
    ctx << "switch (handle)" << "{"
    (0 until numImpls).foreach(x => {
      ctx << s"case $x: { _ | p$x; break; }"
    })
    ctx << "default: { CkAbort(\"abstract proxy unable to find match\"); }"
    ctx << "}"
    ctx << "return _.hash();"
    ctx << "}" 
  }

  def visitAbstractProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val name = nameFor(ctx, x)
    val impls = x.derived.map(nameFor(ctx, _)).toList
    ctx << s"struct $name: public ergoline::hashable" << "{" << s"int handle;" << {
        impls.zipWithIndex.flatMap({
          case (derived, idx) =>
            List(s"$derived p$idx;", s"$name($derived x) : handle($idx), p$idx(x) { }")
        }) ++ List(s"$name() : handle(-1) { }")
    } << visitAbstractPup(ctx, impls.length) << {
      x.members.foreach(x => visitAbstractEntry(ctx, assertValid[EirFunction](x.member), impls.length))
    } << {
      makeHasher(ctx, impls.length)
    } << "};"
  }

  def visitConcreteProxy(ctx: CodeGenerationContext, x: EirProxy): Unit = {
    val base = nameFor(ctx, x.base)
    val name = s"${base}_${x.collective.map(x => s"${x}_").getOrElse("")}"
    GenerateCpp.visitTemplateArgs(ctx, x.templateArgs)
    val args = if (x.templateArgs.nonEmpty) GenerateCpp.templateArgumentsToString(ctx, x.templateArgs) else ""
    ctx << s"struct $name: public CBase_$name$args" << "{" << {
      ctx << "void pup(PUP::er &p)" << "{" << pupperFor(ctx, x, "impl_", x.base) << "}"; ()
    } << {
      x.membersToGen
        .foreach(x => visitProxyMember(ctx, x))
    } << "std::shared_ptr<" << nameFor(ctx, x.base, includeTemplates = true) << ">" << s" impl_;" << s"};"
  }

  def makeArgsVector(ctx: CodeGenerationContext, name: String): Unit = {
    ctx << s"auto $name = std::make_shared<std::vector<std::string>>(msg->argc);" <<
      s"std::transform(msg->argv, msg->argv + msg->argc, $name->begin()," <<
      "[](const char* x) -> std::string { return std::string(x); });"
  }

  def makePointerRhs(ctx: (CodeGenerationContext, EirNode))(current: String, expected: EirType): String = {
    expected match {
      case t: EirTupleType =>
        "std::make_tuple(" + {
          t.children.zipWithIndex.map({
            case (r, idx) =>
              makePointerRhs(ctx)(s"std::get<$idx>($current)", Find.uniqueResolution(r))
          }).mkString(", ")
        } + ")"
      case t if needsCasting(t) =>
        s"ergoline::from_pupable<${qualifiedNameFor(ctx._1, ctx._2)(expected)}>($current)"
      case _ => current
    }
  }

  def makeSmartPointer(ctx: CodeGenerationContext)(x: EirFunctionArgument): Unit = {
    val ty: EirType = Find.uniqueResolution(x.declaredType)
    if (needsCasting(ty)) {
      ctx << ctx.typeFor(ty, Some(x)) << nameFor(ctx, x) << "=" << {
        makePointerRhs((ctx, x))(s"${x.name}_", ty)
      } << ";"
    }
  }

  def needsCasting(t: EirType): Boolean = {
    t match {
      case t: EirTupleType => t.children.map(Find.uniqueResolution[EirType]).exists(needsCasting)
      case _ => t.isTrait && t.isPointer && !t.isSystem
    }
  }

  def visitFunctionArgument(ctx: CodeGenerationContext, arg: EirFunctionArgument): Unit = {
    val ty = Find.uniqueResolution[EirType](arg.declaredType)
    GenerateCpp.visitFunctionArgument(ctx, arg)
    if (needsCasting(ty)) ctx.append("_")
  }

  def visitFunctionArguments(ctx: CodeGenerationContext, args: List[EirFunctionArgument]): Unit = {
    if (args.nonEmpty) {
      for (arg <- args.init) {
        visitFunctionArgument(ctx, arg)
        ctx << ","
      }
      visitFunctionArgument(ctx, args.last)
    }
  }

  def visitProxyMember(ctx: CodeGenerationContext, x: EirMember): Unit = {
      val proxy = x.parent.to[EirProxy]
      val isConstructor = x.isConstructor
      val isCollective = proxy.exists(_.collective.isDefined)
      val index = Option.when(isCollective)("[this->thisIndex]").getOrElse("")
      val base = proxy.map(x => nameFor(ctx, x.base, includeTemplates = true)).getOrElse("")
      val f = assertValid[EirFunction](x.member)
      val isMain = proxy.exists(_.isMain)
      val isAsync = x.annotation("async").isDefined
      val isSingleton = proxy.exists(_.singleton)
      val dropCount = if (isConstructor) 1 else 0
      val args = f.functionArgs.drop(dropCount)
      if (isAsync) ctx << "void"
      else if (!isConstructor) ctx << ctx.typeFor(f.returnType)
      ctx << {
        if (isConstructor) {
          val baseName = proxy.map(x => nameFor(ctx, x.base)).getOrElse("")
          s"${baseName}_${proxy.flatMap(_.collective).map(x => s"${x}_").getOrElse("")}"
        } else {
          nameFor(ctx, f)
        }
      } << "(" << {
        if (isMain && isConstructor) { ctx << "CkArgMsg* msg"; () }
        else {
          if (isAsync) {
            ctx << ctx.typeFor(f.returnType) << temporary(ctx)
            if (args.nonEmpty) ctx << ","
          }
          visitFunctionArguments(ctx, args)
        }
      } << ")" << "{" << {
        if (isConstructor && isMain) args.headOption.foreach(x => makeArgsVector(ctx, x.name))
        else args.foreach(makeSmartPointer(ctx))
      }
      if (isConstructor) {
        ctx << "this->impl_ = std::make_shared<" << base << ">(" <<
          (List(s"this->thisProxy$index") ++ args.map(nameFor(ctx, _))).mkString(", ") << ");"
      } else {
        if (isAsync) {
          ctx << temporary(ctx) << ".set("
        } else if (Find.uniqueResolution(f.returnType) != globals.typeFor(EirLiteralTypes.Unit)) {
          ctx << "return "
        }
        ctx << s"this->impl_->${nameFor(ctx, f)}(${f.functionArgs.map(nameFor(ctx, _)).mkString(", ")})"
        if (isAsync) ctx << ");"
        else ctx << ";"
      }
      ctx << "}"
  }

}
