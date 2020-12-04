package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline._
import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.ast.{EirNamedNode, EirNode}
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

  def unableToUnify(ctx: EirNode, it : Iterable[EirType]): Nothing = {
    Console.err.println(s"${contextualize(ctx)}: could not unify types: ${it.init.map(nameFor) mkString ", "} and ${nameFor(it.last)}")
    exitAction()
  }

  def unableToResolve(resolvable: EirResolvable[_]): Nothing = {
    Console.err.println(s"${contextualize(resolvable)}: unable to resolve $resolvable")
    exitAction()
  }

  def missingType(node: EirNode): Nothing = {
    Console.err.println(s"${contextualize(node)}: could not find the type of $node")
    exitAction()
  }

  def missingNamespace(node: EirNode): Nothing = {
    Console.err.println(s"${contextualize(node)}: could not find the namespace of $node")
    exitAction()
  }
}
