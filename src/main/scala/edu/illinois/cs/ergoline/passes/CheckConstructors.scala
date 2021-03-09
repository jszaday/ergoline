package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

object CheckConstructors {

  def checkConstructorsWithin(module: EirScope): Int = {
    var numChecked = 0
    val classes = Find.within[EirClassLike](module, _ => true)
    val pairs = classes.map(c => {
      (c, c.members.filter(_.isConstructor))
    })
    for ((cls, constructors) <- pairs) {
      numChecked += Math.max(1, checkConstructors(null, cls, constructors))
    }
    numChecked
  }

  def checkConstructors(ctx: TypeCheckContext, cls: EirClassLike): Int = {
    checkConstructors(ctx, cls, cls.members.filter(_.isConstructor))
  }

  def checkConstructors(ctx: TypeCheckContext, cls: EirClassLike, constructors: List[EirMember]): Int = {
    val needsInitialization = cls.needsInitialization
    if (constructors.isEmpty && needsInitialization.nonEmpty) {
      Errors.missingConstructor(cls)
    }
    for (constructor <- constructors) {
      if (!fulfillsSuperConstructor(constructor)) {
        Errors.missingSuperConstructor(constructor)
      }
      if (!selfAssignmentsOk(ctx, cls, constructor)) {
        Errors.invalidSelfAssignment(constructor)
      }
      if (!fulfillsMandatoryAssignments(ctx, needsInitialization, constructor)) {
        Errors.missingMemberAssignment(constructor)
      }
    }
    constructors.length
  }

  // TODO implement this?
  def fulfillsSuperConstructor(constructor: EirMember): Boolean = true

  def selfAssignmentsOk(ctx: TypeCheckContext, cls: EirClassLike, constructor: EirMember): Boolean = {
    val argDeclPairs = constructor.member.asInstanceOf[EirFunction].functionArgs.collect {
      case x@EirFunctionArgument(_, _, _, _, true) => x
    }.map(arg => {
      (cls.findChild[EirMember](withName(arg.name)).headOption, arg)
    })
    argDeclPairs.isEmpty || argDeclPairs.forall(x => x match {
      case (Some(EirMember(_, d: EirDeclaration, _)), arg: EirFunctionArgument) =>
        constructorAssignmentOk(ctx, d, arg.declaredType)
      case _ => false
    })
  }

  private def canAssignHelper(implicit ctx: TypeCheckContext,
                              x: EirResolvable[EirType], y: EirResolvable[EirType]): Boolean = {
    Find.uniqueResolution(x).canAssignTo(Find.uniqueResolution(y))
  }

  def constructorAssignmentOk(implicit ctx: TypeCheckContext,
                              decl: EirDeclaration, declaredType: EirResolvable[EirType]): Boolean = {
    (!decl.isFinal || decl.initialValue.isEmpty) &&
      canAssignHelper(ctx, declaredType, decl.declaredType)
  }

  def fulfillsMandatoryAssignments(implicit ctx: TypeCheckContext,
                                   needsInitialization: List[EirMember], member: EirMember): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case EirFunctionArgument(_, n, t, _, true) =>
          n == decl.name && constructorAssignmentOk(ctx, decl, t)
        case _ => false
      } || Find.within[EirAssignment](constructor, assignmentTargetsDeclaration(_, decl)).nonEmpty
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(x: EirAssignment, d: EirDeclaration) = true
}
