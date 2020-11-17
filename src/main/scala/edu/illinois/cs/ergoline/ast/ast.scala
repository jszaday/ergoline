package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast.types.EirType
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichEirNode

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

  def contains(other: EirNode): Boolean = (other == this) || this.findWithin(other == _).nonEmpty

  override def toString: String = unparse
}

trait EirScope extends EirNode {
  var lastSearch: Int = -1
}

abstract class EirExpressionNode extends EirNode {
  def eirType: EirResolvable[EirType]
}

trait EirNamedNode extends EirNode {
  def name: String

  def fullyQualifiedName: List[String] = parent match {
    case Some(x: EirNamedNode) => x.fullyQualifiedName ++ List(name)
    case _ => List(name)
  }

  override def hashCode(): Int = name.hashCode
}

case class EirBlock(var parent: Option[EirNode], var children: List[EirNode]) extends EirNode with EirScope {
  def findPositionOf(node: EirNode): Option[Int] = {
    children.zipWithIndex.collectFirst({
      case (child, idx) if child.contains(node) => idx
    })
  }
}

case object EirGlobalNamespace extends EirNode with EirScope {
  private val modules: mutable.HashMap[String, EirNamespace] = new mutable.HashMap

  def clear(): Unit = modules.clear()

  def put(name: String, ns: EirNamespace): Option[EirNamespace] = modules.put(name, ns)

  override def parent: Option[EirNode] = None

  override def parent_=(option: Option[EirNode]): Unit = ()

  override def children: Iterable[EirNode] = modules.values
}

case class EirNamespace(var parent: Option[EirNode], var children: List[EirNode], var name: String)
  extends EirNode with EirScope with EirNamedNode {

}

case class EirDeclaration(var parent: Option[EirNode], var isFinal: Boolean, var name: String,
                          var declaredType: EirResolvable[EirType], var initialValue: Option[EirExpressionNode])
  extends EirNamedNode {

  override def children: Iterable[EirNode] = List(declaredType) ++ initialValue
}

trait EirClassLike extends EirNode with EirScope with EirNamedNode with EirType {
  var members: List[EirMember]
  var templateArgs: List[EirTemplateArgument]
  var extendsThis: Option[EirResolvable[EirType]]
  var implementsThese: List[EirResolvable[EirType]]

  override def children: List[EirNode] = templateArgs ++ members
  override def resolved: EirType = this

  def needsInitialization: List[EirMember] =
    members.collect {
      case m@EirMember(_, EirDeclaration(_, true, _, _, None), _) => m
    }
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
  extends EirNode with EirClassLike

case class EirTrait(var parent: Option[EirNode], var members: List[EirMember],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirType]],
                    var implementsThese: List[EirResolvable[EirType]])
  extends EirNode with EirClassLike

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
  extends EirNode with EirScope with EirNamedNode {
  override def children: Iterable[EirNode] = body.map(List(_)).getOrElse(Nil) ++ templateArgs ++ functionArgs
}

case class EirImport(var parent: Option[EirNode], var symbol: EirSymbol[EirScope with EirNamedNode])
  extends EirNode with EirNamedNode with EirScope {
  override def children: Iterable[EirNode] = List(symbol)

  override def name: String = symbol.qualifiedName.head
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

  override def eirType: EirResolvable[EirType] = types.EirTupleType(parent, expressions.map(_.eirType))
}

object EirTupleExpression {
  def fromExpressions(parent: Option[EirNode], expressions: List[EirExpressionNode]): EirExpressionNode =
    expressions match {
      case Nil => globals.unitLiteral(parent)
      case head :: Nil => head
      case lst =>
        val t = EirTupleExpression(parent, lst)
        lst.foreach(_.parent = Some(t))
        t
    }
}

case class EirLambdaExpression(var parent: Option[EirNode], var args: List[EirFunctionArgument], var body: EirBlock)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = args ++ List(body)

  override def eirType: EirResolvable[EirType] = types.EirLambdaType(parent, args.map(_.declaredType), Find.returnType(body))
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
  val String, Integer, Float, Character, Unit = Value
}

case class EirSymbol[+T <: EirNamedNode : Manifest](var parent: Option[EirNode], var qualifiedName: List[String])
  extends EirExpressionNode with EirResolvable[T] {
  override def children: Iterable[EirNode] = Nil

  override def eirType: EirResolvable[EirType] = Find.typeOf(resolved)

  override def resolved: T = Find.fromSymbol(this).headOption
    .getOrElse(throw new RuntimeException(s"could not resolve $this!"))
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

case class EirForLoop(var parent: Option[EirNode], var header: EirForLoopHeader, var body: EirBlock) extends EirNode with EirScope {
  override def children: Iterable[EirNode] = header.children ++ List(body)
}

//case class EirTypeOf(var parent: Option[EirNode], var exprNode: EirExpressionNode) extends EirExpressionNode {
//  override def eirType: EirResolvable[EirType] = exprNode.eirType
//
//  override def children: Iterable[EirNode] = List(exprNode)
//}