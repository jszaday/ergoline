package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFunction, EirNamedNode, EirNode, EirSpecializable, EirTemplateArgument}
import edu.illinois.cs.ergoline.resolution.EirResolvable

object Errors {

  val errorCode: Int = -1

  def contextualize(n: EirNode): String =
    n.location.map(_.toString).getOrElse(n.toString)

  def nameFor(n: EirNode): String = n match {
    case n: EirNamedNode => n.name
    case _ => n.toString
  }

  var exitAction: () => Nothing = () => sys.exit(errorCode)

  def log(msg: String): Unit = if (globals.verbose) println(msg)
  def warn(msg: String): Unit = Console.err.println(s"warning: $msg")

  def cannotCast(ctx: EirNode, a: EirType, b: EirType): Nothing = {
    Console.err.println(s"${contextualize(ctx)}: ${nameFor(a)} cannot be cast to ${nameFor(b)}")
    exitAction()
  }

  def missingField(ctx: EirNode, a: EirType, field: String): Nothing = {
    Console.err.println(s"${contextualize(ctx)}: cannot resolve field '$field' of ${nameFor(a)}")
    exitAction()
  }

  def unableToUnify(ctx: EirNode, a: EirType, b: EirType): Nothing = unableToUnify(ctx, Seq(a, b))

  def missingBody(f: EirFunction): Nothing ={
    Console.err.println(s"${contextualize(f)}: expected body for ${nameFor(f)}")
    exitAction()
  }

  def unableToUnify(ctx: EirNode, it : Iterable[EirType]): Nothing = {
    if (it.isEmpty) missingType(ctx)
    Console.err.println(s"${contextualize(ctx)}: could not unify types: ${it.init.map(nameFor) mkString ", "} and ${nameFor(it.last)}")
    exitAction()
  }

  def unableToResolve(resolvable: EirResolvable[_]): Nothing = {
    Console.err.println(s"${contextualize(resolvable)}: unable to resolve $resolvable")
    exitAction()
  }

  def unableToResolve(s: String): Nothing = {
    Console.err.println(s"unable to resolve $s")
    exitAction()
  }

  def missingType(node: EirNode): Nothing = {
    Console.err.println(s"${contextualize(node)}: could not find the type of $node")
    exitAction()
  }

  def invalidTupleIndices(nodes: Iterable[EirNode]): Nothing = {
    Console.err.println(s"${contextualize(nodes.head)}: (${nodes mkString ", "}) are not proper tuple indices.")
    exitAction()
  }

  def missingNamespace(node: EirNode): Nothing = {
    Console.err.println(s"${contextualize(node)}: could not find the namespace of $node")
    exitAction()
  }

  def invalidParentClass(a: EirClassLike, b: EirClassLike, hint: String = ""): Nothing = {
    Console.err.println(s"${contextualize(a)}: ${nameFor(a)} cannot be used as a parent class for ${nameFor(b)}${if (hint.nonEmpty) ", " + hint else "."}")
    exitAction()
  }

  def missingSpecialization(a: EirNode): Nothing = {
    Console.err.println(s"${contextualize(a)}: missing specialization for ${nameFor(a)}.")
    exitAction()
  }

  def incorrectType(a: EirNode, c: Class[_]): Nothing = {
    Console.err.println(s"${contextualize(a)}: expected an ${c.getName}, got ${nameFor(a)}.")
    exitAction()
  }

  def unknownOperator(a: EirNode, s: String): Nothing = {
    Console.err.println(s"${contextualize(a)}: unrecognized operator $s.")
    exitAction()
  }
}
