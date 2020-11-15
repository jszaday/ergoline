package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.types.EirType
import edu.illinois.cs.ergoline.{globals, types, util}

import scala.collection.mutable

object EirAccessibility extends Enumeration {
  type EirAccessibility = Value
  val Public, Private, Protected = Value
}

abstract class EirNode {
  var parent: Option[EirNode]
  var annotations: List[EirAnnotation] = Nil

  def scope: Option[EirScope] =
    parent flatMap {
      case x: EirScope => Some(x)
      case x: EirNode => x.scope
      case _ => None
    }

  def children: Iterable[EirNode]

  def unparse: String = UnparseAst.visit(this)

  override def toString: String = unparse
}

abstract class EirExpressionNode extends EirNode {
  def eirType: EirResolvable[EirType]
}

abstract class EirScope extends EirNode {
  var cachedSymbols: Option[(Int, Map[String, EirNode])] = None

  def contains(symbol: String): Boolean = symbols.contains(symbol)

  def apply(symbol: String): Option[EirNode] =
    if (contains(symbol)) Some(symbols(symbol)) else None

  def symbols: Map[String, EirNode] =
    (parent match {
      case Some(x: EirScope) => x.symbols
      case _ => Map.empty[String, EirNode]
    }) ++ (cachedSymbols match {
      case Some((hash, cached)) if hash == children.hashCode() => cached
      case _ =>
        cachedSymbols = Some(children.hashCode(), children.collect {
          case x: EirNamedNode => (x.name, x)
        }.toMap)
        cachedSymbols.get._2
    })

  override def scope: Option[EirScope] = Some(this)
}

trait EirNamedNode extends EirNode {
  def name: String

  def fullyQualifiedName: List[String] = parent match {
    case Some(x: EirNamedNode) => x.fullyQualifiedName ++ List(name)
    case _ => List(name)
  }

  override def hashCode(): Int = name.hashCode
}

case class EirBlock(var parent: Option[EirNode], var children: Iterable[EirNode]) extends EirScope

case object EirGlobalNamespace extends EirScope {
  private val modules: mutable.HashMap[String, EirNamespace] = new mutable.HashMap

  def clear(): Unit = modules.clear()

  def put(name: String, ns: EirNamespace): Option[EirNamespace] = modules.put(name, ns)

  override def parent: Option[EirNode] = None

  override def parent_=(option: Option[EirNode]): Unit = ()

  override def symbols: Map[String, EirNode] = modules.toMap

  override def children: Iterable[EirNode] = modules.values
}

case class EirNamespace(var parent: Option[EirNode], var children: List[EirNode], var name: String)
  extends EirScope with EirNamedNode {

}

case class EirDeclaration(var parent: Option[EirNode], var isFinal: Boolean, var name: String,
                          var declaredType: EirResolvable[EirType], var initialValue: Option[EirExpressionNode])
  extends EirNamedNode {

  override def children: Iterable[EirNode] = initialValue.map(List(_)).getOrElse(Nil)
}

trait EirInheritable extends EirNode with EirType {
  var extendsThis: Option[EirResolvable[EirType]]
  var implementsThese: List[EirResolvable[EirType]]

  override def resolve(scope: EirScope): EirType = this

  override def represents: Option[EirNode] = Some(this)

  override def typeChildren: List[EirResolvable[EirType]] = Nil
}

case class EirTemplateArgument(var parent: Option[EirNode], var name: String) extends EirNamedNode {
  var lowerBound: Option[EirResolvable[EirType]] = None
  var upperBound: Option[EirResolvable[EirType]] = None

  override def children: Iterable[EirNode] = Nil
}

case class EirClass(var parent: Option[EirNode], var members: List[EirMember],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirType]],
                    var implementsThese: List[EirResolvable[EirType]])
  extends EirScope with EirNamedNode with EirInheritable {
  override def children: List[EirNode] = members ++ templateArgs

  def needsInitialization: List[EirMember] =
    members.collect {
      case m@EirMember(_, EirDeclaration(_, true, _, _, None), _) => m
    }
}

case class EirTrait(var parent: Option[EirNode], var members: List[EirMember],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirType]],
                    var implementsThese: List[EirResolvable[EirType]])
  extends EirScope with EirNamedNode with EirInheritable {

  override def children: List[EirNode] = members ++ templateArgs
}

case class EirMember(var parent: Option[EirNode], var member: EirNamedNode, var accessibility: EirAccessibility.Value)
  extends EirNamedNode {
  override def name: String = member.name

  def isConstructorOf(other: EirClass): Boolean = parent.contains(other) && isConstructor

  def isConstructor: Boolean = member.isInstanceOf[EirFunction] && parent.map(_.asInstanceOf[EirNamedNode]).exists(_.name == name)

  override def children: Iterable[EirNode] = List(member)
}

case class EirFunction(var parent: Option[EirNode], var body: Option[EirNode],
                       var name: String, var templateArgs: List[EirTemplateArgument],
                       var functionArgs: List[EirFunctionArgument],
                       var returnType: EirResolvable[EirType])
  extends EirScope with EirNamedNode {
  override def children: Iterable[EirNode] = body.map(List(_)).getOrElse(Nil) ++ templateArgs ++ functionArgs
}

case class EirAnnotation(var parent: Option[EirNode], var name: String) extends EirNode {
  override def children: Iterable[EirNode] = Nil
}

case class EirBinaryExpression(var parent: Option[EirNode], var lhs: EirExpressionNode, var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(lhs, rhs)

  override def eirType: EirResolvable[EirType] = ???
}

case class EirUnaryExpression(var parent: Option[EirNode], var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(rhs)

  override def eirType: EirResolvable[EirType] = ???
}

case class EirFunctionArgument(var parent: Option[EirNode], var name: String,
                               var declaredType: EirResolvable[EirType], var isFinal: Boolean, var isSelfAssigning: Boolean)
  extends EirNamedNode {
  override def children: Iterable[EirNode] = Nil
}

case class EirAssignment(var parent: Option[EirNode], var target: EirExpressionNode, var value: EirExpressionNode) extends EirNode {
  override def children: Iterable[EirNode] = List(target, value)
}

case class EirTupleExpression(var parent: Option[EirNode], var expressions: List[EirExpressionNode]) extends EirExpressionNode {
  override def children: Iterable[EirNode] = expressions

  override def eirType: EirResolvable[EirType] = types.EirTupleType(expressions.map(_.eirType))
}

object EirTupleExpression {
  def fromExpressions(parent: Option[EirNode], expressions: List[EirExpressionNode]): EirExpressionNode =
    expressions match {
      case Nil => globals.UnitValue
      case head :: Nil => head
      case lst =>
        val t = EirTupleExpression(parent, lst)
        // TODO should not encounter null
        lst.foreach(x => if (x != null) x.parent = Some(t))
        t
    }
}

case class EirLambdaExpression(var parent: Option[EirNode], var args: List[EirFunctionArgument], var body: EirBlock)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = args ++ List(body)

  override def eirType: EirResolvable[EirType] = types.EirLambdaType(args.map(_.declaredType), Find.returnType(body))
}

case class EirReturn(var parent: Option[EirNode], var expression: EirExpressionNode) extends EirNode {
  override def children: Iterable[EirNode] = List(expression)
}

case class EirTernaryOperator(var parent: Option[EirNode], var test: EirExpressionNode, var ifTrue: EirExpressionNode, var ifFalse: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(test, ifTrue, ifFalse)

  override def eirType: EirResolvable[EirType] = Find.unionType(ifTrue.eirType, ifFalse.eirType)
}

case class EirLiteral(var parent: Option[EirNode], var `type`: EirLiteralTypes.Value, var value: String)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = Nil

  override def eirType: EirResolvable[EirType] = ???
}

object EirLiteralTypes extends Enumeration {
  type EirLiteralTypes = Value
  val String, Integer, Float, Character = Value
}

case class EirSymbol[T](var parent: Option[EirNode], var qualifiedName: List[String])
  extends EirExpressionNode with EirResolvable[T] {
  override def children: Iterable[EirNode] = Nil

  override def eirType: EirResolvable[EirType] = ???

  override def resolve(scope: EirScope): T = ???

  override def represents: Option[EirNode] = Some(this)
}

case class EirFunctionCall(var parent: Option[EirNode], var target: EirExpressionNode, var args: List[EirExpressionNode])
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(target) ++ args

  override def eirType: EirResolvable[EirType] = ???
}

case class EirFieldAccessor(var parent: Option[EirNode], var target: EirExpressionNode, var field: String)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(target)

  override def eirType: EirResolvable[EirType] = ???
}

case class EirArrayReference(var parent: Option[EirNode], var target: EirExpressionNode, var args: List[EirExpressionNode])
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(target) ++ args

  override def eirType: EirResolvable[EirType] = ???
}

case class EirTypeCast(var parent: Option[EirNode], var to: EirResolvable[EirType], var value: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(value)

  override def eirType: EirResolvable[EirType] = to
}

trait EirForLoopHeader {
  def children: List[EirNode]
}

case class EirCStyleHeader(var declaration: Option[EirDeclaration], var test: Option[EirExpressionNode], var increment: Option[EirAssignment]) extends EirForLoopHeader {
  override def children: List[EirNode] = List(declaration, test, increment).collect({
    case Some(x) => x
  })
}

case class EirForAllHeader(var parent: Option[EirNode], var identifiers: List[String], var expressionNode: EirExpressionNode) extends EirForLoopHeader {
  override def children: List[EirNode] = declarations

  def declarations: List[EirDeclaration] = ???
}

case class EirForLoop(var parent: Option[EirNode], var header: EirForLoopHeader, var body: EirBlock) extends EirScope {
  override def children: Iterable[EirNode] = header.children ++ List(body)
}

//case class EirTypeOf(var parent: Option[EirNode], var exprNode: EirExpressionNode) extends EirExpressionNode {
//  override def eirType: EirResolvable[EirType] = exprNode.eirType
//
//  override def children: Iterable[EirNode] = List(exprNode)
//}