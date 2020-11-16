package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption}

object CheckConstructors {

  def checkAllConstructors(): Int = {
    var numChecked = 0
    for ((cls, constructors) <- constructorsByClass) {
      val needsInitialization = cls.needsInitialization
      assert(constructors.nonEmpty || needsInitialization.isEmpty)
      for (constructor <- constructors) {
        assert(fulfillsSuperConstructor(constructor))
        assert(selfAssignmentsOk(cls, constructor))
        assert(fulfillsMandatoryAssignments(needsInitialization, constructor))
      }
      numChecked += Math.max(1, constructors.length)
    }
    numChecked
  }

  def constructorsByClass: List[(EirClass, List[EirMember])] =
    classes.map(c => {
      (c, Find.matching[EirMember]({
        case x: EirMember if x.isConstructorOf(c) => x
      }, c).toList)
    }).toList

  def classes: Iterable[EirClass] = Find.all[EirClass](EirGlobalNamespace)

  def fulfillsSuperConstructor(constructor: EirMember): Boolean = {
    true
  }

  def selfAssignmentsOk(cls: EirClass, constructor: EirMember): Boolean = {
    val argDeclPairs = constructor.member.asInstanceOf[EirFunction].functionArgs.collect {
      case x@EirFunctionArgument(_, _, _, _, true) => x
    }.map(arg => {
      (cls.findWithin[EirMember](withName(arg.name)).headOption, arg)
    })
    argDeclPairs.isEmpty || argDeclPairs.forall(x => x match {
      case (Some(EirMember(_, d: EirDeclaration, _)), arg: EirFunctionArgument) =>
        constructorAssignmentOk(d, arg.declaredType)
      case _ => false
    })
  }

  def constructorAssignmentOk(decl: EirDeclaration, declaredType: EirResolvable[EirType]): Boolean = {
    !decl.isFinal || decl.initialValue.isEmpty
  }

  def fulfillsMandatoryAssignments(needsInitialization: List[EirMember], member: EirMember): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case EirFunctionArgument(_, n, t, _, true) =>
          n == decl.name && constructorAssignmentOk(decl, t)
        case _ => false
      } || Find.matching[EirAssignment]({
        case x: EirAssignment if assignmentTargetsDeclaration(x, decl) => x
      }, constructor).nonEmpty
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(x: EirAssignment, d: EirDeclaration) = true
}
