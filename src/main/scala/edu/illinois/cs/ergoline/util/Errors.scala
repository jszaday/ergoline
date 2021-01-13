package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline._
import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.ast.{EirAwait, EirClassLike, EirExpressionNode, EirFunction, EirMember, EirNamedNode, EirNode, EirSpecializable, EirTemplateArgument}
import edu.illinois.cs.ergoline.resolution.EirResolvable
import org.antlr.v4.runtime.tree.ParseTree

object Errors {

  val errorCode: Int = -1

  def contextualize(n: EirNode): String =
    n.location.map(_.toString).getOrElse(n.toString)

  def nameFor(n: EirNode): String = n match {
    case n: EirNamedNode => n.name
    case t: EirTupleType => s"(${t.children.map(nameFor(_)) mkString ", "})"
    case _ => n.toString
  }


  def format(ctx: EirNode, msg: String, params: Any*): String = {
    val start = Option(ctx).map(contextualize).getOrElse("???")
    start + ": " + msg.format(params.map {
      case n: EirNode => nameFor(n)
      case x => x.toString
    }: _*)
  }

  private var exitAction: String => Nothing = (s: String) => {
    Console.err.println(s)
    sys.exit(errorCode)
  }

  def log(msg: String): Unit = if (globals.verbose) println(msg)
  def warn(msg: String): Unit = Console.err.println(s"warning: $msg")
  def exit(msg: String): Nothing = exitAction(msg)

  def useDebugAction(): Unit = {
    exitAction = (s: String) => throw new RuntimeException(s)
  }

  def cannotCast(ctx: EirNode, a: EirType, b: EirType): Nothing = {
    exitAction(format(ctx, "%s cannot be cast to %s", a, b))
  }

  def missingField(ctx: EirNode, a: EirType, field: String): Nothing = {
    exitAction(format(ctx, "cannot resolve field '%s' of %s", field, a))
  }

  def unableToUnify(ctx: EirNode, a: EirType, b: EirType): Nothing = unableToUnify(ctx, Seq(a, b))

  def unableToName(n: EirNode): Nothing ={
    exitAction(format(n, "unable to generate name for %s", n))
  }

  def missingBody(f: EirFunction): Nothing ={
    exitAction(format(f, "expected body for %s", f))
  }

  def unableToUnify(ctx: EirNode, it : Iterable[EirType]): Nothing = {
    if (it.isEmpty) missingType(ctx)
    else exitAction(format(ctx, "could not unify types: [%s]", it.map(nameFor) mkString ", "))
  }

  def unableToResolve(resolvable: EirResolvable[_]): Nothing = {
    exitAction(format(resolvable, "unable to resolve %s", resolvable))
  }

  def unableToResolve(s: String): Nothing = {
    exitAction(format(null, "unable to resolve %s", s))
  }

  def missingType(node: EirNode): Nothing = {
    exitAction(format(node, "could not find the type of %s", node))
  }

  def inaccessibleMember(target: EirMember, usage: EirNode): Nothing = {
    exitAction(format(target, "%s is not accessible within %s", target, usage))
  }
  def invalidTupleIndices(nodes: Iterable[EirNode]): Nothing = {
    exitAction(format(nodes.head, "%s are not valid tuple indices", nodes mkString ", "))
  }

  def missingNamespace(node: EirNode): Nothing = {
    exitAction(format(node, "could not find the namespace of %s", node))
  }

  def invalidParentClass(a: EirClassLike, b: EirClassLike, hint: String = ""): Nothing = {
    exitAction(format(a, "%s cannot be used as a parent class for %s %s", a, b, if (hint.nonEmpty) ", " + hint else "."))
  }

  def missingSpecialization(a: EirNode): Nothing = {
    exitAction(format(a, "missing specialization for %s", a))
  }

  def incorrectType(a: EirNode, c: Class[_]): Nothing = {
    exitAction(format(a, "expected a(n) %s, instead got %s (a(n) %s)", c.getName, a, a.getClass.getName))
  }

  def unknownOperator(a: EirNode, s: String): Nothing = {
    exitAction(format(a, "unrecognized operator %s", s))
  }

  def expectedSync(a: EirAwait, t: EirExpressionNode): Nothing = {
    exitAction(format(a, "await must target @sync method (instead got: %s)", t))
  }

  def cannotSerialize(ctx: EirNode, t: EirType): Nothing = {
    exitAction(format(ctx, "cannot pup/serialize transient type %s", t))
  }

  def ambiguousOverload(a: EirFunction, b: EirFunction): Nothing = {
    exitAction(format(a, "%s is potentially ambiguous with %s", a, b))
  }

  def cannotParse(tree: ParseTree): Nothing = {
    exitAction(format(null, "could not parse %s", tree))
  }

  def systemFnHasBody(f: EirFunction): Nothing = {
    exitAction(format(f, "system functions cannot have a body"))
  }

  def bodyLessFunction(f: EirFunction): Nothing = {
    exitAction(format(f, "expected a body for function %s", f))
  }

  def doesNotOverride(f: EirFunction): Nothing = {
    exitAction(format(f, "%s marked override but does not override anything", f))
  }

  def expectedOverride(a: EirFunction, b: EirMember): Nothing = {
    exitAction(format(a, "%s not marked override but overrides %s", a, b))
  }

  def incompatibleOverride(f: EirFunction, a: EirType, b: EirType): Nothing = {
    exitAction(format(f, "%s has an incompatible return type with overridden function (%s vs. %s)", f, a, b))
  }
}
