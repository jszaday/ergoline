package edu.illinois.cs.ergoline.ast

import java.io.File

import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.resolution.{EirResolvable, Find, Modules}
import edu.illinois.cs.ergoline.{globals, util}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichEirNode, RichOption}

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

  def contains(other: EirNode): Boolean = (other == this) || this.findWithin(other == _).nonEmpty

  override def toString: String = unparse

  def unparse: String = UnparseAst.visit(this)

  override def hashCode(): Int = children.hashCode

  def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean

  override def equals(obj: Any): Boolean = {
    // node comparisons should _generally_ be strict
    // they may explode otherwise due to their recursive relationships
    // besides, they should usually be singletons :)
    this.asInstanceOf[AnyRef].eq(obj.asInstanceOf[AnyRef])
  }
}

abstract class EirUserNode extends EirNode {
  def accept[T](visitor: EirVisitor[T]): T
}

trait EirEncloseExempt extends EirNode

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

trait EirSimpleContainer extends EirNode with EirScope {
  var children: List[EirNode]

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    util.updateWithin(children, oldValue, newValue).map(children = _).isDefined
  }
}

case class EirBlock(var parent: Option[EirNode], var children: List[EirNode]) extends EirSimpleContainer {
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

  def apply(name: String): Option[EirNamespace] =
    Option.when(modules.contains(name))(modules(name))

  override def parent: Option[EirNode] = None

  override def parent_=(option: Option[EirNode]): Unit = ()

  override def children: Iterable[EirNode] = modules.values

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirNamespace(var parent: Option[EirNode], var children: List[EirNode], var name: String)
  extends EirSimpleContainer with EirNamedNode {
  // TODO this should probably be a standard Node function/more sophisticated (i.e. indicate no match found)
  def removeChild(node : EirNode): Unit = {
    children = children.filter(_ != node)
  }
}

case class EirDeclaration(var parent: Option[EirNode], var isFinal: Boolean, var name: String,
                          var declaredType: EirResolvable[EirType], var initialValue: Option[EirExpressionNode])
  extends EirNamedNode {

  override def children: Iterable[EirNode] = List(declaredType) ++ initialValue

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    (initialValue.contains(oldValue) && util.applyOrFalse[EirExpressionNode](x => initialValue = Some(x), newValue)) ||
    ((oldValue == declaredType) && util.applyOrFalse[EirResolvable[EirType]](declaredType = _, newValue))
  }
}

// NOTE this should be enclose exempt and only creatable through a factory
case class EirFileSymbol(var parent : Option[EirNode], var file : File)
  extends EirScope with EirNamedNode with EirResolvable[EirNode] with EirEncloseExempt {
  var _resolved : Option[EirNode] = None

  override def resolve(): EirNode = {
    if (_resolved.isEmpty) {
      _resolved = parent.to[EirScope].map(Modules.load(file, _))
    }
    _resolved match {
      case Some(x) => x
      case _ => throw new RuntimeException(s"could not resolve $file!")
    }
  }

  override def resolved: Boolean = _resolved.nonEmpty

  override def children: Iterable[EirNode] = _resolved

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def name: String = Modules.expectation(file)

  override def toString: String = s"UNLOADED($name)"
}

trait EirClassLike extends EirNode with EirScope with EirNamedNode with EirType {
  var members: List[EirMember]
  var templateArgs: List[EirTemplateArgument]
  var extendsThis: Option[EirResolvable[EirType]]
  var implementsThese: List[EirResolvable[EirType]]
  var specializations: List[EirTemplatedType] = Nil

  def putSpecialization(x : EirTemplatedType): Unit = {
    if (templateArgs.length != x.args.length) {
      throw new RuntimeException(s"invalid template specialization: $x")
    }
    specializations +:= x
  }

  def member(name: String): Option[EirMember] = members.find(_.name == name)

  override def children: List[EirNode] = templateArgs ++ extendsThis ++ implementsThese ++ members

  def needsInitialization: List[EirMember] =
    members.collect {
      case m@EirMember(_, EirDeclaration(_, true, _, _, None), _) => m
    }

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    (extendsThis.contains(oldValue) && util.applyOrFalse[EirResolvable[EirType]](x => extendsThis = Some(x), newValue)) ||
      util.updateWithin(templateArgs, oldValue, newValue).map(templateArgs = _).isDefined ||
      util.updateWithin(implementsThese, oldValue, newValue).map(implementsThese = _).isDefined ||
      util.updateWithin(members, oldValue, newValue).map(members = _).isDefined
  }
}

case class EirTemplateArgument(var parent: Option[EirNode], var name: String) extends EirNamedNode {
  var lowerBound: Option[EirResolvable[EirType]] = None
  var upperBound: Option[EirResolvable[EirType]] = None

  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = false
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
  def isConstructorOf(other: EirClass): Boolean = parent.contains(other) && isConstructor

  def isConstructor: Boolean = member.isInstanceOf[EirFunction] && parent.map(_.asInstanceOf[EirNamedNode]).exists(_.name == name)

  override def name: String = member.name

  override def children: Iterable[EirNode] = List(member)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (member == oldNode) && util.applyOrFalse[EirNamedNode](member = _, newNode)
  }
}

case class EirFunction(var parent: Option[EirNode], var body: Option[EirBlock],
                       var name: String, var templateArgs: List[EirTemplateArgument],
                       var functionArgs: List[EirFunctionArgument],
                       var returnType: EirResolvable[EirType])
  extends EirNode with EirScope with EirNamedNode {
  override def children: Iterable[EirNode] = body.toList ++ templateArgs ++ functionArgs :+ returnType

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    if (body.contains(oldNode)) {
      util.applyOrFalse[EirBlock](x => body = Some(x), newNode)
    }
    else if (returnType == oldNode) {
      util.applyOrFalse[EirResolvable[EirType]](returnType = _, newNode)
    } else {
      util.updateWithin(templateArgs, oldNode, newNode).map(templateArgs = _).orElse(
        util.updateWithin(functionArgs, oldNode, newNode).map(functionArgs = _)
      ).isDefined
    }
  }
}

//case class EirSystemFunction(parent: Option[EirNode], name : String, templateArgs: List[EirTemplateArgument],
//                              functionArgs: List[EirFunctionArgument], returnType: EirResolvable[EirType])
//  extends EirNode with EirNamedNode {
//  override def children: Iterable[EirNode] = None
//
//  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
//}

case class EirImport(var parent: Option[EirNode], var qualified: List[String])
  extends EirResolvable[EirNode] with EirScope with EirEncloseExempt {
  var _resolved : Option[EirScope] = None

  def wildcard: Boolean = qualified.last == "_"

  override def children: Iterable[EirNode] = {
    if (wildcard) _resolved.toIterable.flatMap(_.children)
    else _resolved
  }

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def resolve(): EirNode = {
    _resolved = Modules(if (wildcard) qualified.init else qualified, EirGlobalNamespace).to[EirScope]
    _resolved match {
      case Some(x) => x
      case _ => throw new RuntimeException("could not resolve import!")
    }
  }

  override def resolved: Boolean = _resolved.nonEmpty
}

case class EirAnnotation(var parent: Option[EirNode], var name: String) extends EirNode {
  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
}

case class EirBinaryExpression(var parent: Option[EirNode], var lhs: EirExpressionNode, var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(lhs, rhs)

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    if (lhs == oldNode) util.applyOrFalse[EirExpressionNode](lhs = _, newNode)
    else if (rhs == oldNode) util.applyOrFalse[EirExpressionNode](rhs = _, newNode)
    else false
  }
}

case class EirUnaryExpression(var parent: Option[EirNode], var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(rhs)

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode == rhs) && util.applyOrFalse[EirExpressionNode](rhs = _, newNode)
  }
}

case class EirFunctionArgument(var parent: Option[EirNode], var name: String,
                               var declaredType: EirResolvable[EirType], var isFinal: Boolean, var isSelfAssigning: Boolean)
  extends EirNamedNode {
  override def children: Iterable[EirNode] = Seq(declaredType)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode == declaredType) && util.applyOrFalse[EirResolvable[EirType]](declaredType = _, newNode)
  }
}

case class EirAssignment(var parent: Option[EirNode], var lval: EirExpressionNode, var rval: EirExpressionNode) extends EirNode {
  override def children: Iterable[EirNode] = Seq(lval, rval)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((lval == oldNode) && util.applyOrFalse[EirExpressionNode](lval = _, newNode)) ||
      ((rval == oldNode) && util.applyOrFalse[EirExpressionNode](rval = _, newNode))
  }
}

case class EirTupleExpression(var parent: Option[EirNode], var expressions: List[EirExpressionNode]) extends EirExpressionNode {
  override def children: Iterable[EirNode] = expressions

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    util.updateWithin(expressions, oldNode, newNode).map(expressions = _).isDefined
  }
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

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    util.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
      ((body == oldNode) && util.applyOrFalse[EirBlock](body = _, newNode))
  }
}

// TODO expression should be an optional!
case class EirReturn(var parent: Option[EirNode], var expression: EirExpressionNode) extends EirNode {
  override def children: Iterable[EirNode] = List(expression)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode == expression) && util.applyOrFalse[EirExpressionNode](expression = _, newNode)
  }
}

case class EirTernaryOperator(var parent: Option[EirNode], var test: EirExpressionNode, var ifTrue: EirExpressionNode, var ifFalse: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(test, ifTrue, ifFalse)

  override def eirType: EirResolvable[EirType] = ??? // Find.unionType(ifTrue.eirType, ifFalse.eirType)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((test == oldNode) && util.applyOrFalse[EirExpressionNode](test = _, newNode)) ||
      ((ifTrue == oldNode) && util.applyOrFalse[EirExpressionNode](x => ifTrue = x, newNode)) ||
      ((ifFalse == oldNode) && util.applyOrFalse[EirExpressionNode](x => ifFalse = x, newNode))
  }
}

case class EirLiteral(var parent: Option[EirNode], var `type`: EirLiteralTypes.Value, var value: String)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = Nil

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
}

object EirLiteralTypes extends Enumeration {
  type EirLiteralTypes = Value
  val String = Value("string")
  val Integer = Value("int")
  val Float = Value("float")
  val Character = Value("char")
  val Unit = Value("unit")
}

case class EirSymbol[T <: EirNamedNode : Manifest](var parent: Option[EirNode], var qualifiedName: List[String])
  extends EirExpressionNode with EirResolvable[T] {

  private var _resolved : Option[List[T]] = None

  override def children: Iterable[EirNode] = Nil

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def resolve(): T = {
    if (_resolved.isEmpty) {
      _resolved = Some(Find.fromSymbol(this).toList)
    }
    _resolved match {
      case Some(x) => x.head
      case None => throw new RuntimeException(s"could not resolve $this!")
    }
  }

  def candidates: List[T] = _resolved.getOrElse(Nil)

  override def resolved: Boolean = _resolved.isDefined
}

trait EirPostfixExpression extends EirExpressionNode {
  var target: EirExpressionNode
  var args: List[EirExpressionNode]

  override def children: Iterable[EirNode] = target +: args

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    util.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
      ((target == oldNode) && util.applyOrFalse[EirExpressionNode](target = _, newNode))
  }
}

case class EirFunctionCall(var parent: Option[EirNode], var target: EirExpressionNode, var args: List[EirExpressionNode])
  extends EirPostfixExpression {
  override def eirType: EirResolvable[EirType] = ???
}

case class EirArrayReference(var parent: Option[EirNode], var target: EirExpressionNode, var args: List[EirExpressionNode])
  extends EirPostfixExpression {
  override def eirType: EirResolvable[EirType] = ???
}

case class EirFieldAccessor(var parent: Option[EirNode], var target: EirExpressionNode, var field: String)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = Seq(target)

  override def eirType: EirResolvable[EirType] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (target == oldNode) && util.applyOrFalse[EirExpressionNode](target = _, newNode)
  }
}

case class EirTypeCast(var parent: Option[EirNode], var to: EirResolvable[EirType], var value: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = Seq(to, value)

  override def eirType: EirResolvable[EirType] = to

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((to == oldNode) && util.applyOrFalse[EirResolvable[EirType]](to = _, newNode)) ||
      ((value == oldNode) && util.applyOrFalse[EirExpressionNode](x => value = x, newNode))
  }
}

trait EirForLoopHeader {
  def children: Iterable[EirNode]
  def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean
}

case class EirCStyleHeader(var declaration: Option[EirDeclaration], var test: Option[EirExpressionNode], var increment: Option[EirAssignment]) extends EirForLoopHeader {
  override def children: Iterable[EirNode] = declaration ++ test ++ increment

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (declaration.contains(oldNode) && util.applyOrFalse[EirDeclaration](x => declaration = Some(x), newNode)) ||
      (test.contains(oldNode) && util.applyOrFalse[EirExpressionNode](x => test = Some(x), newNode)) ||
      (increment.contains(oldNode) && util.applyOrFalse[EirAssignment](x => increment = Some(x), newNode))
  }
}

case class EirForAllHeader(var parent: Option[EirNode], var identifiers: List[String], var expression: EirExpressionNode) extends EirForLoopHeader {
  override def children: Iterable[EirNode] = declarations

  def declarations: List[EirDeclaration] = ???

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (expression == oldNode) && util.applyOrFalse[EirExpressionNode](expression = _, newNode)
  }
}

case class EirForLoop(var parent: Option[EirNode], var header: EirForLoopHeader, var body: EirBlock) extends EirNode with EirScope {
  override def children: Iterable[EirNode] = header.children ++ List(body)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((body == oldNode) && util.applyOrFalse[EirBlock](body = _, newNode)) ||
      header.replaceChild(oldNode, newNode)
  }
}

//case class EirTypeOf(var parent: Option[EirNode], var exprNode: EirExpressionNode) extends EirExpressionNode {
//  override def eirType: EirResolvable[EirType] = exprNode.eirType
//
//  override def children: Iterable[EirNode] = List(exprNode)
//}