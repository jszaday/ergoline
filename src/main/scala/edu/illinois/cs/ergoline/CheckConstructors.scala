package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichAllowed, RichOption}

object CheckConstructors {

  def classes: Iterable[EirClass] = util.findAll[EirClass](EirGlobalNamespace)

  def constructorsByClass: List[(EirClass, List[EirMember])] =
    classes.map(c => {
      (c, util.findMatching[EirMember]({
        case x: EirMember if x.isConstructorOf(c) => x
      }, c).toList)
    }).toList

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

  def fulfillsSuperConstructor(constructor: EirMember): Boolean = {
    true
  }

  def constructorAssignmentOk(decl: EirDeclaration, declaredType: types.Allowed): Boolean = {
    declaredType.canAssignTo(decl.declaredType) && (!decl.isFinal || decl.initialValue.isEmpty)
  }

  def selfAssignmentsOk(cls : EirClass, constructor : EirMember): Boolean = {
    val argDeclPairs = constructor.member.asInstanceOf[EirFunction].functionArgs.collect {
      case x@EirFunctionArgument(_, _, _, _, true) => x
    }.map(arg => {
      (util.find[EirMember](List(cls.name, arg.name), cls.parent.to[EirScope]), arg)
    })
    argDeclPairs.isEmpty || argDeclPairs.forall(x => x match {
      case (Some(EirMember(_, d: EirDeclaration, _)), arg: EirFunctionArgument) =>
        constructorAssignmentOk(d, arg.declaredType)
      case _ => false
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(x: EirAssignment, d: EirDeclaration) = true

  def fulfillsMandatoryAssignments(needsInitialization : List[EirMember], member: EirMember): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case EirFunctionArgument(_, n, t, _, true) =>
          n == decl.name && constructorAssignmentOk(decl, t)
        case _ => false
      } || util.findMatching[EirAssignment]({
        case x: EirAssignment if assignmentTargetsDeclaration(x, decl) => x
      }, constructor).nonEmpty
    })
  }
}
