package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.resolution.EirResolvable
import edu.illinois.cs.ergoline.resolution.Find.asClassLike
import edu.illinois.cs.ergoline.util.Errors
import edu.illinois.cs.ergoline.util.TypeCompatibility.RichEirClassLike

object CheckClasses {

  final case class ClassCheckException(node: EirClassLike, message: String) extends Exception(message)

  var checked: List[EirClassLike] = Nil

  /* necessary checks:
   * - ensure it only "implements" traits
   * - ensure implementation of parent's abstract functions
   * - ensure overriding functions are marked as such
   * - ensure parent classes' constructor is called
   * - ensure traits do not have constructors or declarations
   */
  def visit(ctx: TypeCheckContext, node: EirClassLike): Unit = {
    if (checked.contains(node)) return

    node match {
      case c : EirClass => visitClass(ctx, c)
      case t : EirTrait => visitTrait(t)
    }

    node.inherited.foreach(x =>
      checkParentClass(ctx, node, CheckTypes.visit(ctx, x)))

    checked +:= node
  }

  def error(node: EirClassLike, message: String): Any = {
    throw ClassCheckException(node, message)
  }

  def addDerived(ctx: TypeCheckContext, a: EirClassLike, b: EirClassLike): Unit = {
    b.inherited.map(CheckTypes.visit(ctx, _)).foreach(x => addDerived(ctx, a, asClassLike(x)))
    b.derived = b.derived + a
  }

  def checkParentClass(ctx: TypeCheckContext, node: EirClassLike, candidate: EirType): Unit = {
    implicit val tyCtx: TypeCheckContext = ctx
    val resolved = asClassLike(candidate)
    node match {
      case _: EirTrait if !resolved.isInstanceOf[EirTrait] =>
        Errors.invalidParentClass(node, resolved, "traits can only extend/implement traits.")
      case _: EirClass if node.extendsThis.contains(candidate) && !resolved.isInstanceOf[EirClass] =>
        Errors.invalidParentClass(node, resolved, "classes cannot extend non-classes.")
      case _ =>
    }
    if (resolved.isDescendantOf(node)) {
      Errors.invalidParentClass(node, resolved, "circular relationship.")
    }
    val others = node.inherited.map(CheckTypes.visit(ctx, _)).filterNot(_ == candidate).map(asClassLike)
    val found = others.find(x => x == resolved)
    if (found.isDefined) {
      Errors.invalidParentClass(node, resolved, s"already implemented by ${found.get.name}.")
    }
    addDerived(ctx, node, resolved)
    // NOTE typechecking is used to catch mismatches in our parents' template specialization
    //      i.e. missing/wrong number of arguments
  }

  def visitTrait(node : EirTrait): Unit = {
    node.members.foreach(x => {
      x.member match {
        case _ : EirFunction => if (x.isConstructor) error(node, "cannot have constructor")
        case x => error(node, s"invalid member $x")
      }
    })
  }

  def visitClass(ctx: TypeCheckContext, node : EirClass): Unit = {
    CheckConstructors.checkConstructors(ctx, node)
  }
}
