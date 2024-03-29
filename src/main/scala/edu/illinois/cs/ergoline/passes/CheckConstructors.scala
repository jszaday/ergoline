package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirType

object CheckConstructors {
  def mkDefaultConstructor(cls: EirClassLike): EirMember = {
    val m = EirMember(Some(cls), null, EirAccessibility.Public)
    val f = EirFunction(
      Some(m),
      None,
      "self",
      Nil,
      Nil,
      Nil,
      globals.unitType,
      None
    )
    f.body = Some(EirBlock(Some(f), Nil))
    m.member = f
    m
  }

  def checkConstructors(cls: EirClassLike)(implicit
      ctx: TypeCheckContext
  ): Int = {
    def constructors: Seq[EirMember] = cls.members.filter(_.isConstructor)
    val needsInitialization = cls.needsInitialization

    if (constructors.isEmpty) {
      if (needsInitialization.nonEmpty) {
        Errors.missingConstructor(cls)
      } else if (cls.annotation("unconstructable").isEmpty) {
        cls.members +:= mkDefaultConstructor(cls)
      }
    }

    val reqsDefaultCons = cls match {
      case c: EirClass => c.objectType
      case _           => false
    }

    if (
      reqsDefaultCons && !constructors.exists({
        case EirMember(_, f: EirFunction, _) => f.functionArgs.isEmpty
        case _                               => false
      })
    ) {
      Errors.expectedDefaultConstructible(cls)
    }

    for (constructor <- constructors) {
      (
        cls,
        cls.extendsThis.map(CheckTypes.visit).flatMap(Find.tryClassLike)
      ) match {
        case (base: EirClass, Some(parent: EirClass)) =>
          if (!fulfillsSuperConstructor(base, parent, constructor)) {
            Errors.missingSuperConstructor(constructor)
          }
        case _ =>
      }
      if (!selfAssignmentsOk(cls, constructor)) {
        Errors.invalidSelfAssignment(constructor)
      }
      if (
        !fulfillsMandatoryAssignments(
          needsInitialization,
          constructor
        ) && !constructor.isSystem
      ) {
        Errors.missingMemberAssignment(constructor)
      }
    }

    constructors.length
  }

  // TODO implement this?
  def fulfillsSuperConstructor(
      parent: EirClass,
      base: EirClass,
      constructor: EirMember
  ): Boolean = {
    val parentCons = parent.members.filter(_.isConstructor)

    parentCons.isEmpty || (parentCons collect {
      case EirMember(_, f: EirFunction, _) => f
    } exists { x => x.functionArgs.isEmpty })
  }

  def selfAssignmentsOk(cls: EirClassLike, constructor: EirMember)(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    val argDeclPairs = constructor.member
      .asInstanceOf[EirFunction]
      .functionArgs
      .collect {
        case x if x.isSelfAssigning => x
      }
      .map(arg => {
        (Find.child[EirMember](cls, withName(arg.name)).headOption, arg)
      })
    argDeclPairs.forall(x =>
      x match {
        case (
              Some(EirMember(_, d: EirDeclaration, _)),
              arg: EirFunctionArgument
            ) => constructorAssignmentOk(d, arg.declaredType)
        case _ => false
      }
    )
  }

  private def canAssignHelper(
      x: EirResolvable[EirType],
      y: EirResolvable[EirType]
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    val xx = CheckTypes.visit(x)
    val yy = CheckTypes.visit(y)
    xx.canAssignTo(yy)
  }

  def constructorAssignmentOk(
      decl: EirDeclaration,
      declaredType: EirResolvable[EirType]
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    (!decl.isFinal || decl.initialValue.isEmpty) && canAssignHelper(
      declaredType,
      decl.declaredType
    )
  }

  def fulfillsMandatoryAssignments(
      needsInitialization: List[EirMember],
      member: EirMember
  )(implicit
      ctx: TypeCheckContext
  ): Boolean = {
    val constructor = member.member.asInstanceOf[EirFunction]
    needsInitialization.isEmpty || needsInitialization.forall(variable => {
      val decl = variable.member.asInstanceOf[EirDeclaration]
      constructor.functionArgs.exists {
        case x: EirFunctionArgument
            if x.isSelfAssigning && x.name == decl.name =>
          constructorAssignmentOk(decl, x.declaredType)
        case _ => false
      } || Find
        .within[EirAssignment](
          constructor,
          assignmentTargetsDeclaration(_, decl)
        )
        .nonEmpty
    })
  }

  // TODO actually implement this
  private def assignmentTargetsDeclaration(
      x: EirAssignment,
      d: EirDeclaration
  ) = true
}
