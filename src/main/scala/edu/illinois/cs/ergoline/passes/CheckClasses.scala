package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.resolution.Find

import scala.annotation.tailrec

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
  def visit(node: EirClassLike): Unit = {
    if (checked.contains(node)) return

    CheckConstructors.checkConstructors(node)

    val traits = node match {
      case c : EirClass => visitClass(c)
      case t : EirTrait => visitTrait(t)
    }

    checkImplements(node, traits)

    traits.foreach(asClassLike(_).derived ++= List(node))

    checked +:= node
  }

  def error(node: EirClassLike, message: String): Any = {
    throw ClassCheckException(node, message)
  }

  def checkImplements(node : EirClassLike, implements: List[EirType]): Unit = {
    // ensure there are no repeated elements
    if (implements.length != implements.distinct.length) {
      error(node, "cannot have duplicate parent traits")
    }
    // ensure only traits are implemented
    implements.foreach({
      case t : EirTrait =>
        if (t.templateArgs.nonEmpty)
          error(node, s"$t expected specialization")
      case t : EirTemplatedType =>
        val base = Find.uniqueResolution(t.base)
        base match {
          case _ : EirTrait =>
          case x => error(node, s"cannot extend/implement $x")
        }
      case x => error(node, s"cannot extend/implement $x")
    })
  }

  def checkOverride(parent: EirMember, child: EirMember): Unit = {

  }

  def visitTrait(node : EirTrait): List[EirType] = {
    node.members.foreach(x => {
      x.member match {
        case _ : EirFunction => if (x.isConstructor) error(node, "cannot have constructor")
        case x => error(node, s"cannot have member $x")
      }
    })
    Find.uniqueResolution(node.extendsThis ++ node.implementsThese).toList
  }

  def visitClass(node : EirClass): List[EirType] = {
    val base: Option[EirType] = node.extendsThis.map(Find.uniqueResolution[EirType])
    base.foreach(asClassLike(_).derived ++= List(node))
    val traits: List[EirType] = Find.uniqueResolution(node.implementsThese).toList
    traits
  }

  @tailrec
  def asClassLike(node : EirNode): EirClassLike = {
    node match {
      case t: EirTemplatedType => asClassLike(Find.uniqueResolution(t.base))
      case c: EirClassLike => c
      case _ => throw new RuntimeException(s"$node is not a class-like type")
    }
  }
}
