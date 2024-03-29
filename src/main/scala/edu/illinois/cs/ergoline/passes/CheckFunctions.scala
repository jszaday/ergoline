package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirAccessibility.Private
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFunction, EirMember}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirResolvable
import edu.illinois.cs.ergoline.util.{Errors, assertValid, sweepInheritedFirst}

object CheckFunctions {

  // TODO this needs to consider template arguments and parameter packs
  //      consider: def fn<A...>(A... as);
  //           and: def fn(int i);
  //      these are ambiguous in situ!
  def sharedArgs(implicit
      ctx: TypeCheckContext,
      a: EirFunction,
      b: EirFunction
  ): Boolean = {
    (a.templateArgs.isEmpty && b.templateArgs.isEmpty) && {
      a.parent
        .to[EirMember]
        .zip(b.parent.to[EirMember])
        .forall({ case (a, b) => a.isStatic == b.isStatic })
    } && {
      val as = CheckTypes.visit(a.functionArgs)
      val bs = CheckTypes.visit(b.functionArgs)
      CheckTypes.argumentsMatch(as, bs, exact = true)
    }
  }

  private def overridesWithin(
      ctx: TypeCheckContext,
      within: EirClassLike,
      of: EirFunction
  ): Option[(EirMember, EirType)] = {
    sweepInheritedFirst(
      ctx,
      within,
      (ictx: TypeCheckContext, base: EirClassLike) => {
        Find
          .child[EirMember](base, withName(of.name))
          .collectFirst {
            case m @ EirMember(_, f: EirFunction, accessibility)
                if accessibility != Private && sharedArgs(ictx, of, f) =>
              (m, CheckTypes.visit(f.returnType)(ictx))
          }
          .orElse(overridesWithin(ictx, base, of))
      }
    )
  }

  def seekOverrides(
      ctx: TypeCheckContext,
      member: EirMember
  ): Option[(EirMember, EirType)] = {
    overridesWithin(ctx, member.base, assertValid[EirFunction](member.member))
  }

  // TODO check self-assigning arguments?
  def visit(implicit ctx: TypeCheckContext, function: EirFunction): Unit = {
    val member = function.parent.to[EirMember]
    val constructor = member.exists(_.isConstructor)

    val system = member.getOrElse(function).annotation("system")
    val mailbox = member.exists(_.isMailbox)
    val hasBody = function.body.isDefined

    val bodyAllowed = !mailbox && system.isEmpty
    val bodyOptional =
      !bodyAllowed || (!constructor && member.exists(_.base.isAbstract))

    if (hasBody && !bodyAllowed) {
      Errors.systemFnHasBody(function)
    } else if (!hasBody && !bodyOptional) {
      Errors.bodyLessFunction(function)
    }

    if (constructor) {
      return
    } else if (function.functionArgs.exists(_.isSelfAssigning)) {
      Errors.invalidSelfAssignment(function)
    }

    // only check overloads for non-constructors
    val overloads = Find.overloads(function)

    member.foreach(_.hasOverloads = overloads.nonEmpty)

    overloads
      .filter(sharedArgs(ctx, function, _))
      .foreach(Errors.ambiguousOverload(function, _))

    val isOverride = member.exists(_.isOverride)
    val found = member.flatMap(seekOverrides(ctx, _))
    found match {
      case Some((m: EirMember, theirs: EirType)) =>
        if (!isOverride) {
          Errors.expectedOverride(function, m)
        } else {
          // TODO add warning about overriding templated methods
          val ours = CheckTypes.visit(function.returnType)
          if (!ours.canAssignTo(theirs)) {
            Errors.incompatibleOverride(function, ours, theirs)
          }
        }
      case None if isOverride => Errors.doesNotOverride(function)
      case _                  => // OK
    }

    // TODO check if @entry outside of proxy?
  }
}
