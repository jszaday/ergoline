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
      numChecked += Math.max(1, checkConstructors(cls, constructors))
    }
    numChecked
  }

  def checkConstructors(cls: EirClassLike): Int = {
    checkConstructors(cls, cls.members.filter(_.isConstructor))
  }

  def checkConstructors(cls: EirClassLike, constructors: List[EirMember]): Int = {
    val needsInitialization = cls.needsInitialization
    if (constructors.isEmpty && needsInitialization.nonEmpty) {
      throw new RuntimeException(s"$cls does not fulfill all mandatory assignments")
    }
    for (constructor <- constructors) {
      if (!fulfillsSuperConstructor(constructor)) {
        throw new RuntimeException(s"$constructor does not fulfill all super constructors")
      }
      if (!selfAssignmentsOk(cls, constructor)) {
        throw new RuntimeException(s"${Errors.contextualize(constructor)} contains an invalid self-assignment")
      }
      if (!fulfillsMandatoryAssignments(needsInitialization, constructor)) {
        throw new RuntimeException(s"$constructor does not fulfill all mandatory assignments")
      }
    }
    constructors.length
  }

  def fulfillsSuperConstructor(constructor: EirMember): Boolean = {
    true
  }

  def selfAssignmentsOk(cls: EirClassLike, constructor: EirMember): Boolean = {
    val argDeclPairs = constructor.member.asInstanceOf[EirFunction].functionArgs.collect {
      case x@EirFunctionArgument(_, _, _, _, true) => x
    }.map(arg => {
      (cls.findChild[EirMember](withName(arg.name)).headOption, arg)
    })
    argDeclPairs.isEmpty || argDeclPairs.forall(x => x match {
      case (Some(EirMember(_, d: EirDeclaration, _)), arg: EirFunctionArgument) =>
        constructorAssignmentOk(d, arg.declaredType)
      case _ => false
    })
  }

  private def canAssignHelper(x: EirResolvable[EirType], y: EirResolvable[EirType]): Boolean = {
    Find.uniqueResolution(x).canAssignTo(Find.uniqueResolution(y))
  }

  def constructorAssignmentOk(decl: EirDeclaration, declaredType: EirResolvable[EirType]): Boolean = {
    (!decl.isFinal || decl.initialValue.isEmpty) &&
      canAssignHelper(declaredType, decl.declaredType)
  }

  def fulfillsMandatoryAssignments(needsInitialization: List[EirMember], member: EirMember): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case EirFunctionArgument(_, n, t, _, true) =>
          n == decl.name && constructorAssignmentOk(decl, t)
        case _ => false
      } || constructor.findWithin[EirAssignment](assignmentTargetsDeclaration(_, decl)).nonEmpty
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(x: EirAssignment, d: EirDeclaration) = true
}
