package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirFunction, EirMember}
import edu.illinois.cs.ergoline.resolution.Find
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption

object CheckFunctions {

  def sharedArgs(ctx: TypeCheckContext, a: EirFunction, b: EirFunction): Boolean = {
    (a.templateArgs.isEmpty && b.templateArgs.isEmpty) && {
      val as = CheckTypes.visit(ctx, a.functionArgs)
      val bs = CheckTypes.visit(ctx, b.functionArgs)
      CheckTypes.argumentsMatch(as, bs, exact = true)
    }
  }

  def visit(ctx: TypeCheckContext, function: EirFunction): Unit = {
    val overloads = Find.overloads(function)
    val system = function.annotation("system")
    val hasBody = function.body.isDefined
    val member = function.parent.to[EirMember]

    if (system.exists(_ => hasBody)) {
      Errors.systemFnHasBody(function)
    } else if (!hasBody && !member.exists(_.base.isAbstract)) {
      Errors.bodyLessFunction(function)
    }

    overloads
      .find(sharedArgs(ctx, function, _))
      .foreach(Errors.ambiguousOverload(function, _))
  }
}
