package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

object CheckConstructors {
  def checkConstructors(cls: EirClassLike)(implicit ctx: TypeCheckContext): Int = {
    checkConstructors(cls, cls.members.filter(_.isConstructor))
  }

  def checkConstructors(cls: EirClassLike, constructors: List[EirMember])
                       (implicit ctx: TypeCheckContext): Int = {
    val needsInitialization = cls.needsInitialization
    if (constructors.isEmpty && needsInitialization.nonEmpty) {
      Errors.missingConstructor(cls)
    }
    for (constructor <- constructors) {
      if (!fulfillsSuperConstructor(constructor)) {
        Errors.missingSuperConstructor(constructor)
      }
      if (!selfAssignmentsOk(cls, constructor)) {
        Errors.invalidSelfAssignment(constructor)
      }
      if (!fulfillsMandatoryAssignments(needsInitialization, constructor)) {
        Errors.missingMemberAssignment(constructor)
      }
    }
    constructors.length
  }

  // TODO implement this?
  def fulfillsSuperConstructor(constructor: EirMember): Boolean = true

  def selfAssignmentsOk(cls: EirClassLike, constructor: EirMember)
                       (implicit ctx: TypeCheckContext): Boolean = {
    val argDeclPairs = constructor.member.asInstanceOf[EirFunction].functionArgs.collect {
      case x@EirFunctionArgument(_, _, _, _, true) => x
    }.map(arg => {
      (Find.child[EirMember](cls, withName(arg.name)).headOption, arg)
    })
    argDeclPairs.isEmpty || argDeclPairs.forall(x => x match {
      case (Some(EirMember(_, d: EirDeclaration, _)), arg: EirFunctionArgument) =>
        constructorAssignmentOk(d, arg.declaredType)
      case _ => false
    })
  }

  private def canAssignHelper(x: EirResolvable[EirType], y: EirResolvable[EirType])
                             (implicit ctx: TypeCheckContext): Boolean = {
    CheckTypes.visit(x).canAssignTo(CheckTypes.visit(y))
  }

  def constructorAssignmentOk(decl: EirDeclaration, declaredType: EirResolvable[EirType])
                             (implicit ctx: TypeCheckContext): Boolean = {
    (!decl.isFinal || decl.initialValue.isEmpty) && canAssignHelper(declaredType, decl.declaredType)
  }

  def fulfillsMandatoryAssignments(needsInitialization: List[EirMember], member: EirMember)
                                  (implicit ctx: TypeCheckContext): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case EirFunctionArgument(_, n, t, _, true) =>
          n == decl.name && constructorAssignmentOk(decl, t)
        case _ => false
      } || Find.within[EirAssignment](constructor, assignmentTargetsDeclaration(_, decl)).nonEmpty
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(x: EirAssignment, d: EirDeclaration) = true
}
