package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirAccessibility.Private
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFunction, EirMember}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.{Errors, assertValid, resolveToPair}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirResolvable

object CheckFunctions {

  // TODO this needs to consider template arguments and parameter packs
  //      consider: def fn<A...>(A... as);
  //           and: def fn(int i);
  //      these are ambiguous in situ!
  def sharedArgs(ctx: TypeCheckContext, a: EirFunction, b: EirFunction): Boolean = {
    (a.templateArgs.isEmpty && b.templateArgs.isEmpty) && {
      val as = CheckTypes.visit(ctx, a.functionArgs)
      val bs = CheckTypes.visit(ctx, b.functionArgs)
      CheckTypes.argumentsMatch(as, bs, exact = true)
    }
  }

  private def overridesWithin(ctx: TypeCheckContext, within: EirClassLike, of: EirFunction): Option[(EirMember, EirType)] = {
    def seek(base: EirClassLike) = {
      Find.child[EirMember](base, withName(of.name)).collectFirst{
        case m@EirMember(_, f: EirFunction, accessibility) if accessibility != Private && sharedArgs(ctx, of, f) =>
          (m, CheckTypes.visit(ctx, f.returnType))
      }.orElse(overridesWithin(ctx, base, of))
    }

    val inherited = within.inherited.map(resolveToPair)
    inherited.flatMap {
      case (base, None) => seek(base)
      case (base, Some(sp)) =>
        val spec = ctx.specialize(base, sp)
        val found = seek(base)
        ctx.leave(spec)
        found
    }.headOption
  }

  def seekOverrides(ctx: TypeCheckContext, member: EirMember): Option[(EirMember, EirType)] = {
    overridesWithin(ctx, member.base, assertValid[EirFunction](member.member))
  }

  def visit(ctx: TypeCheckContext, function: EirFunction): Unit = {
    val overloads = Find.overloads(function)
    val member = function.parent.to[EirMember]
    val system = member.getOrElse(function).annotation("system")
    val hasBody = function.body.isDefined

    if (system.isDefined) {
      if (hasBody) Errors.systemFnHasBody(function)
    } else if (!hasBody && !member.exists(_.base.isAbstract)) {
      Errors.bodyLessFunction(function)
    }

    overloads
      .find(sharedArgs(ctx, function, _))
      .foreach(Errors.ambiguousOverload(function, _))

    val isOverride = member.exists(_.isOverride)
    val found = member.flatMap(seekOverrides(ctx, _))
    found match {
      case Some((m: EirMember, theirs: EirType)) =>
        if (!isOverride) {
          Errors.expectedOverride(function, m)
        } else {
          val ours = CheckTypes.visit(ctx, function.returnType)
          if (!ours.canAssignTo(theirs)) {
            Errors.incompatibleOverride(function, ours, theirs)
          }
        }
      case None if isOverride => Errors.doesNotOverride(function)
      case _ => // OK
    }

    // TODO check if @entry outside of proxy?
  }
}
