package edu.illinois.cs.ergoline.util

import edu.illinois.cs.ergoline._
import edu.illinois.cs.ergoline.ast.types.{EirTupleType, EirType}
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import org.antlr.v4.runtime.tree.ParseTree

import scala.reflect.ClassTag

object Errors {

  val errorCode: Int = -1
  private var exitAction: String => Nothing = (s: String) => {
    Console.err.println(s)
    sys.exit(errorCode)
  }

  def contextualize(n: EirNode): String =
    n.location.map(_.toString).getOrElse("???")

  def nameFor(n: EirNode): String =
    n match {
      case n: EirNamedNode => n.name
      case t: EirTupleType => s"(${t.children.map(nameFor(_)) mkString ", "})"
      case _               => n.toString
    }

  def format(ctx: EirNode, msg: String, params: Any*): String = {
    val start = Option(ctx).map(contextualize).getOrElse("???")
    start + ": " + msg.format(params.map {
      case n: EirNode => nameFor(n)
      case x          => x.toString
    }: _*)
  }

  def log(msg: String): Unit = if (globals.verbose) println(msg)

  def warn(msg: String): Unit = Console.err.println(s"warning: $msg")

  def exit(msg: String): Nothing = exitAction(msg)

  final case class EirException(msg: String) extends RuntimeException(msg)

  def useDebugAction(): Unit = {
    exitAction = (s: String) => throw EirException(s)
  }

  def unreachable(): Nothing = {
    useDebugAction()
    exit("this should be unreachable, you may need to report this issue!")
  }

  def assignToVal(node: EirAssignment): Nothing = {
    exit(format(node, "assignment to val (%s)", node.lval))
  }

  def cannotCast(ctx: EirNode, a: EirType, b: EirType): Nothing = {
    exit(format(ctx, "%s cannot be cast to %s", a, b))
  }

  def missingField(ctx: EirNode, a: EirType, field: String): Nothing = {
    exit(format(ctx, "cannot resolve field '%s' of %s", field, a))
  }

  def unableToUnify(ctx: EirNode, a: EirType, b: EirType): Nothing =
    unableToUnify(ctx, Seq(a, b))

  def unableToName(n: EirNode): Nothing = {
    exit(format(n, "unable to generate name for %s", n))
  }

  def missingBody(f: EirFunction): Nothing = {
    exit(format(f, "expected body for %s", f))
  }

  def unableToUnify(ctx: EirNode, it: Iterable[EirType]): Nothing = {
    if (it.isEmpty) missingType(ctx)
    else
      exit(
        format(
          ctx,
          "could not unify types: [%s]",
          it.map(nameFor) mkString ", "
        )
      )
  }

  def unableToResolve(resolvable: EirResolvable[_]): Nothing = {
    exit(format(resolvable, "unable to resolve %s", resolvable))
  }

  def unableToResolve(s: String): Nothing = {
    exit(format(null, "unable to resolve %s", s))
  }

  def missingType(node: EirNode): Nothing = {
    exit(format(node, "could not find the type of %s", node))
  }

  def inaccessibleMember(target: EirMember, usage: EirNode): Nothing = {
    exit(format(target, "%s is not accessible within %s", target, usage))
  }

  def invalidTupleIndices(
      tuple: EirTupleType,
      nodes: Iterable[EirNode]
  ): Nothing = {
    exit(
      format(
        nodes.headOption.orNull,
        "cannot use %s as indices for %s",
        nodes,
        tuple
      )
    )
  }

  def invalidConstExpr(expr: EirNode): Nothing = {
    exit(format(expr, "could not statically determine the value of `%s`", expr))
  }

  def missingNamespace(node: EirNode): Nothing = {
    exit(format(node, "could not find the namespace of %s", node))
  }

  def invalidParentClass(
      a: EirClassLike,
      b: EirClassLike,
      hint: String = ""
  ): Nothing = {
    exit(
      format(
        a,
        "%s cannot be used as a parent class for %s %s",
        a,
        b,
        if (hint.nonEmpty) ", " + hint else "."
      )
    )
  }

  def missingSpecialization(a: EirNode): Nothing = {
    exit(format(a, "missing specialization for %s", a))
  }

  def incorrectType(a: EirNode, c: Class[_]): Nothing = {
    exit(
      format(
        a,
        "expected a(n) %s, instead got %s (a(n) %s)",
        c.getName,
        a,
        a.getClass.getName
      )
    )
  }

  def incorrectType(a: EirNode, c: ClassTag[_]): Nothing =
    incorrectType(a, c.runtimeClass)

  def unknownOperator(a: EirNode, s: String): Nothing = {
    exit(format(a, "unrecognized operator %s", s))
  }

  def expectedSync(a: EirAwait, t: EirExpressionNode): Nothing = {
    exit(format(a, "await must target @sync method (instead got: %s)", t))
  }

  def cannotSerialize(ctx: EirNode, t: EirType): Nothing = {
    exit(format(ctx, "cannot pup/serialize transient type %s", t))
  }

  def ambiguousOverload(a: EirFunction, b: EirFunction): Nothing = {
    exit(format(a, "%s is potentially ambiguous with %s", a, b))
  }

  def cannotParse(tree: ParseTree): Nothing = {
    exit(
      format(
        null,
        "could not parse %s (a(n) %s)",
        Option(tree).map(_.getText).getOrElse("null"),
        Option(tree).map(_.getClass.getName).getOrElse("null")
      )
    )
  }

  def systemFnHasBody(f: EirFunction): Nothing = {
    exit(format(f, "system functions cannot have a body"))
  }

  def bodyLessFunction(f: EirFunction): Nothing = {
    exit(format(f, "expected a body for function %s", f))
  }

  def doesNotOverride(f: EirFunction): Nothing = {
    exit(format(f, "%s marked override but does not override anything", f))
  }

  def expectedOverride(a: EirFunction, b: EirMember): Nothing = {
    exit(format(a, "%s not marked override but overrides %s", a, b))
  }

  def incompatibleOverride(f: EirFunction, a: EirType, b: EirType): Nothing = {
    exit(
      format(
        f,
        "%s has an incompatible return type with overridden function (%s vs. %s)",
        f,
        a,
        b
      )
    )
  }

  def missingConstructor(cls: EirClassLike): Nothing = {
    exit(format(cls, "%s does not have any constructors", cls))
  }

  def missingMemberAssignment(cons: EirMember): Nothing = {
    exit(
      format(cons, "%s needs to initialize all members of %s", cons, cons.base)
    )
  }

  def invalidSelfAssignment(f: EirMember): Nothing = {
    exit(format(f, "%s contains an invalid argument member-initializer", f))
  }

  def missingSuperConstructor(cons: EirMember): Nothing = {
    exit(format(cons, "%s must call super constructor", cons))
  }

  def expectedParameterPack(expansion: EirPackExpansion): Nothing = {
    exit(format(expansion, "expected %s to be a parameter pack", expansion))
  }

  def expectedCallback(node: EirNode): Nothing = {
    exit(format(node, "unsure how to use %s as a callback", node))
  }

  def expectedReducer(node: EirNode): Nothing = {
    exit(format(node, "unsure how to use %s as a reducer", node))
  }

  def expectedLvalue(node: EirExpressionNode): Nothing = {
    exit(format(node, "expected an lvalue but got %s instead", node))
  }

  def unboundSlice(node: EirSlice, ty: Option[EirType]): Nothing = {
    exit(
      format(
        node,
        "unbound slice (%s) of unbound type %s (of unknown size)",
        node,
        ty
      )
    )
  }

  def expectedValueType(node: EirExpressionNode, ty: EirType): Nothing = {
    exit(
      format(
        node,
        "expected a value type, but %s has type %s instead",
        node,
        ty
      )
    )
  }
}
