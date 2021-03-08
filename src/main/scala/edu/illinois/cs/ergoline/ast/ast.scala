package edu.illinois.cs.ergoline.ast

import java.io.File

import edu.illinois.cs.ergoline.ast.types.{EirTemplatedType, EirType}
import edu.illinois.cs.ergoline.passes.UnparseAst
import edu.illinois.cs.ergoline.proxies.{EirProxy, ProxyManager}
import edu.illinois.cs.ergoline.resolution.Find.withName
import edu.illinois.cs.ergoline.resolution.{EirPlaceholder, EirResolvable, Find, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{AstManipulation, Errors}
import edu.illinois.cs.ergoline.{globals, util}

import scala.collection.mutable
import scala.reflect.ClassTag

object EirAccessibility extends Enumeration {
  type EirAccessibility = Value
  val Public = Value(0)
  val Protected = Value(1)
  val Private = Value(2)

  def compatible(a: Value, b: Value): Boolean = {
    a.id >= b.id
  }
}

class EirSourceInfo(var sourceName: String, var line: Int, var start: Int, var text: String) {
  override def toString: String = s"$sourceName:$line:$start"
}

abstract class EirNode {
  var parent: Option[EirNode]
  var annotations: List[EirAnnotation] = Nil
  var _location: Option[EirSourceInfo] = None

  def location: Option[EirSourceInfo] =
    _location.orElse(parent.flatMap(_.location))

  def location_=(location: Option[EirSourceInfo]): Unit = _location = location

  def annotation(name : String): Option[EirAnnotation] =
    annotations.find(_.name == name)

  def scope: Option[EirScope] =
    parent flatMap {
      case x: EirScope => Some(x)
      case x: EirNode => x.scope
      case _ => None
    }

  def children: Iterable[EirNode]

  def contains(other: EirNode): Boolean = {
    (other == this) || Find.ancestors(other).contains(this)
  }

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
  def accept[Context, Value](context: Context, visitor: EirVisitor[Context, Value]): Value
}

trait EirEncloseExempt extends EirNode
trait EirScope extends EirNode

abstract class EirExpressionNode extends EirNode {
  var disambiguation: Option[EirNode] = None
  var foundType: Option[EirType] = None
}

trait EirNamedNode extends EirNode {
  def name: String

  def fullyQualifiedName: List[String] = parent match {
    case Some(x: EirNamespace) => x.fullyQualifiedName ++ List(name)
    case _ => List(name)
  }

  override def hashCode(): Int = name.hashCode
}

trait EirSimpleContainer extends EirNode with EirScope {
  var children: List[EirNode]

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    AstManipulation.updateWithin(children, oldValue, newValue).map(children = _).isDefined
  }
}

case class EirBlock(var parent: Option[EirNode], var children: List[EirNode]) extends EirSimpleContainer {
  def findPositionOf(node: EirNode): Option[Int] = {
    val ancestors = Find.ancestors(node)
    (node +: ancestors).sliding(2).collectFirst{
      case prev :: curr :: _ if curr == this => children.indexOf(prev)
    }
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

  override def children: Iterable[EirNamespace] = modules.values

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode, newNode) match {
      case (x : EirNamespace, y: EirNamespace) if modules.contains(x.name) =>
        modules(x.name) = y
        true
      case _ => false
    }
  }
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

  // NOTE this _might_ infinitely recurse for self so we skip declType
  override def children: Iterable[EirNode] = Iterable(declaredType) ++ initialValue

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    (initialValue.contains(oldValue) && util.applyOrFalse[EirExpressionNode](x => initialValue = Some(x), newValue)) ||
    ((oldValue == declaredType) && util.applyOrFalse[EirResolvable[EirType]](declaredType = _, newValue))
  }
}

// NOTE this should be enclose exempt and only creatable through a factory
case class EirFileSymbol(var parent : Option[EirNode], var file : File)
  extends EirScope with EirNamedNode with EirResolvable[EirNode] with EirEncloseExempt {
  var _resolved : Option[EirNode] = None

  override def resolve(): List[EirNode] = {
    if (_resolved.isEmpty) {
      _resolved = parent.to[EirScope].map(Modules.load(file, _))
    }
    _resolved match {
      case Some(x) => List(x)
      case _ => Errors.unableToResolve(this)
    }
  }

  override def resolved: Boolean = _resolved.nonEmpty

  override def children: Iterable[EirNode] = _resolved

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def name: String = Modules.expectation(file)

  override def toString: String = s"UNLOADED($name)"
}

trait EirSpecializable extends EirNode {
  var templateArgs: List[EirTemplateArgument]
}

trait EirSpecialization extends EirNode {
  def types: List[EirResolvable[EirType]]
}

case class EirSyntheticSpecialization(var types: List[EirResolvable[EirType]]) extends EirSpecialization {
  override var parent: Option[EirNode] = None
  override def children: Iterable[EirNode] = types
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

object EirClassLike {
  def makeSelfDeclaration(parent: Option[EirClassLike], name: String, resolvable: EirResolvable[EirType]): EirMember = {
    val m = EirMember(parent, null, EirAccessibility.Private)
    val d = EirDeclaration(Some(m), isFinal = true, name, resolvable, None)
    m.member = d
    m
  }
}

trait EirClassLike extends EirNode with EirScope with EirNamedNode with EirType with EirSpecializable {
  var isAbstract: Boolean = false
  private var _derived: Set[EirClassLike] = Set()

  def asType: EirResolvable[EirType] = {
    EirPlaceholder[EirType](None, Some({
      if (templateArgs.isEmpty) this
      else EirTemplatedType(None, this, templateArgs)
    }))
  }

  // TODO eventually traits will need a self as well
  def selfDeclarations: List[EirMember] =
    Option.when(this.isInstanceOf[EirClass])({
      EirClassLike.makeSelfDeclaration(Some(this), "self", asType)
    }).toList

  def inherited: Iterable[EirResolvable[EirType]] = extendsThis ++ implementsThese

  def derived: Set[EirClassLike] = _derived
  def derived_=(x : Set[EirClassLike]): Unit = _derived = x

  var members: List[EirMember]
  var extendsThis: Option[EirResolvable[EirType]]
  var implementsThese: List[EirResolvable[EirType]]

  def isDescendantOf(other: EirClassLike): Boolean = {
    inherited.map({
      case t: EirTemplatedType => Find.uniqueResolution(t.base)
      case t => Find.uniqueResolution(t)
    }).collect({
      case c: EirClassLike => c
    }).exists(x => { x == other || x.isDescendantOf(other)})
  }

  def member(name: String): Option[EirMember] = members.find(_.name == name)

  override def children: List[EirNode] = templateArgs ++ extendsThis ++ implementsThese ++ members

  def needsInitialization: List[EirMember] =
    members.collect {
      case m@EirMember(_, EirDeclaration(_, true, _, _, None), _) => m
    }

  override def replaceChild(oldValue: EirNode, newValue: EirNode): Boolean = {
    (extendsThis.contains(oldValue) && util.applyOrFalse[EirResolvable[EirType]](x => extendsThis = Some(x), newValue)) ||
      AstManipulation.updateWithin(templateArgs, oldValue, newValue).map(templateArgs = _).isDefined ||
      AstManipulation.updateWithin(implementsThese, oldValue, newValue).map(implementsThese = _).isDefined ||
      AstManipulation.updateWithin(members, oldValue, newValue).map(members = _).isDefined
  }
}

case class EirTemplateArgument(var parent: Option[EirNode], var name: String)
  extends EirType with EirNamedNode {
  var lowerBound: Option[EirResolvable[EirType]] = None
  var upperBound: Option[EirResolvable[EirType]] = None
  var defaultValue: Option[EirResolvable[EirType]] = None
  var argumentType: Option[EirResolvable[EirType]] = None
  var isPack: Boolean = false

  def hasDefaultValue: Boolean = defaultValue.nonEmpty

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
  extends EirNode with EirClassLike {
  isAbstract = true
}

case class EirMember(var parent: Option[EirNode], var member: EirNamedNode, var accessibility: EirAccessibility.Value)
  extends EirNamedNode {
  var isOverride: Boolean = false
  var isStatic: Boolean = false
  var counterpart: Option[EirMember] = None
  private var entryOnly: Boolean = false
  private var _hasOverloads: Boolean = false

  def hasOverloads_=(yes: Boolean): Unit = _hasOverloads = yes
  def hasOverloads: Boolean = _hasOverloads || counterpart.exists(_.hasOverloads)

  def ordinal: Option[Int] = parent.to[EirClassLike].map(_.members.indexOf(this)).find(_ >= 0)

  def isMailbox: Boolean = annotations.exists(_.name == "mailbox")
  def isEntryOnly: Boolean = entryOnly || isMailbox

  def isImplOnly: Boolean =
    member match {
      case _: EirFunction => !isEntryOnly
      case _: EirDeclaration => true
      case _ => ???
    }

  def makeEntryOnly(): Unit = {
    entryOnly = true
    counterpart.foreach(_.makeEntryOnly())
  }

  def isPublic: Boolean = accessibility == EirAccessibility.Public

  def base: EirClassLike = parent match {
    case Some(p: EirProxy) => ProxyManager.elementFor(p).getOrElse(p)
    case Some(c: EirClassLike) => c
    case _ => Errors.missingType(this)
  }

  def selfDeclarations: List[EirMember] = {
    member match {
      case _: EirFunction if !isStatic => base.selfDeclarations
      case _ => Nil
    }
  }

  def isConstructor: Boolean =
    member.isInstanceOf[EirFunction] &&
      (parent.map(_.asInstanceOf[EirNamedNode]).exists(_.name == name) ||
        parent.to[EirProxy].exists(_.baseName == name))

  // TODO also ensure return type is "unit" unless a/sync or local
  def isEntry: Boolean = member match {
    case _: EirFunction => isMailbox || annotations.exists(_.name == "entry")
    case _ => false
  }

  def isFinal: Boolean = member match {
    case d : EirDeclaration => d.isFinal
    case _ => true
  }

  // TODO these checks should be more robust
//  def isConst: Boolean = member match {
//    case f : EirFunction =>
//      f.functionArgs.headOption.filter(arg => arg.name == "self").exists(_.isFinal)
//    case _ => false
//  }

  def isVirtual: Boolean = member match {
    case _ : EirFunction =>
      (base.isAbstract || isOverride) && !annotations.exists(_.name == "system")
    case _ => false
  }

  override def name: String = member.name

  override def children: Iterable[EirNode] = selfDeclarations :+ member

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (member == oldNode) && util.applyOrFalse[EirNamedNode](member = _, newNode)
  }
}

case class EirFunction(var parent: Option[EirNode], var body: Option[EirBlock],
                       var name: String, var templateArgs: List[EirTemplateArgument],
                       var functionArgs: List[EirFunctionArgument],
                       var returnType: EirResolvable[EirType])
  extends EirNode with EirScope with EirNamedNode with EirSpecializable {
  override def children: Iterable[EirNode] = body.toList ++ templateArgs ++ functionArgs :+ returnType

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    if (body.contains(oldNode)) {
      util.applyOrFalse[EirBlock](x => body = Some(x), newNode)
    }
    else if (returnType == oldNode) {
      util.applyOrFalse[EirResolvable[EirType]](returnType = _, newNode)
    } else {
      AstManipulation.updateWithin(templateArgs, oldNode, newNode).map(templateArgs = _).orElse(
        AstManipulation.updateWithin(functionArgs, oldNode, newNode).map(functionArgs = _)
      ).isDefined
    }
  }
}

case class EirImport(var parent: Option[EirNode], var qualified: List[String])
  extends EirResolvable[EirNode] with EirScope with EirEncloseExempt {
  var _resolved : Option[EirScope] = None

  def wildcard: Boolean = qualified.last == "_"

  override def children: Iterable[EirNode] = {
    if (_resolved.isEmpty) Nil
    else resolve()
  }

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def resolve(): List[EirNode] = {
    val name = Option.when(qualified.length > 1)(qualified.init).getOrElse(qualified)
    val last = Option.when(!wildcard && qualified.length > 1)(qualified.last)
    if (_resolved.isEmpty) {
      _resolved = Modules(name, EirGlobalNamespace).to[EirScope]
    }
    _resolved match {
      case Some(x) =>
        last.map(n => Find.child(x, withName(n))).map(_.toList)
          .getOrElse(if (wildcard) x.children.toList else List(x))
      case _ => Errors.unableToResolve(this)
    }
  }

  override def resolved: Boolean = _resolved.nonEmpty
}

case class EirAnnotation(var name: String, var opts: Map[String, EirLiteral]) extends EirNode with EirEncloseExempt {

  def apply(name: String): Option[EirLiteral] = opts.get(name)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
  override var parent: Option[EirNode] = None
  override def children: Iterable[EirNode] = Nil
}

case class EirBinaryExpression(var parent: Option[EirNode], var lhs: EirExpressionNode, var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(lhs, rhs)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    if (lhs == oldNode) util.applyOrFalse[EirExpressionNode](lhs = _, newNode)
    else if (rhs == oldNode) util.applyOrFalse[EirExpressionNode](rhs = _, newNode)
    else false
  }
}

case class EirUnaryExpression(var parent: Option[EirNode], var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(rhs)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode == rhs) && util.applyOrFalse[EirExpressionNode](rhs = _, newNode)
  }
}

case class EirFunctionArgument(var parent: Option[EirNode], var name: String,
                               var declaredType: EirResolvable[EirType], var isExpansion: Boolean, var isSelfAssigning: Boolean)
  extends EirNamedNode {
  override def children: Iterable[EirNode] = Seq(declaredType)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (oldNode == declaredType) && util.applyOrFalse[EirResolvable[EirType]](declaredType = _, newNode)
  }

  def cloneWith(other: Option[EirNode]): EirFunctionArgument = {
    EirFunctionArgument(other, name, declaredType, isExpansion, isSelfAssigning)
  }
}

case class EirAssignment(var parent: Option[EirNode], var lval: EirExpressionNode, var op: String, var rval: EirExpressionNode) extends EirNode {
  var isValueInitializer: Boolean = false

  override def children: Iterable[EirNode] = Seq(lval, rval)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((lval == oldNode) && util.applyOrFalse[EirExpressionNode](lval = _, newNode)) ||
      ((rval == oldNode) && util.applyOrFalse[EirExpressionNode](rval = _, newNode))
  }
}

case class EirTupleExpression(var parent: Option[EirNode], var expressions: List[EirExpressionNode]) extends EirExpressionNode {
  override def children: Iterable[EirNode] = expressions

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    AstManipulation.updateWithin(expressions, oldNode, newNode).map(expressions = _).isDefined
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

  def captures: List[EirNamedNode] = {
    val predicate = (x: EirNode) => x match {
      case s: EirSymbol[_] =>
        val resolution = Find.uniqueResolution(s)
        Some(resolution match {
          case f: EirFunctionArgument => !f.parent.contains(this)
          case d: EirDeclaration => !Find.ancestors(d).contains(this)
          case EirMember(_, _: EirDeclaration, _) => true
          case _ => false
        })
      case _ => Some(false)
    }
    Find.descendant(body, predicate)
      .map(x => Find.uniqueResolution(x.asInstanceOf[EirResolvable[EirNamedNode]]))
      .toList.distinct.sortBy(_.name)
  }

  override def children: Iterable[EirNode] = args ++ List(body)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    AstManipulation.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
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

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((test == oldNode) && util.applyOrFalse[EirExpressionNode](test = _, newNode)) ||
      ((ifTrue == oldNode) && util.applyOrFalse[EirExpressionNode](x => ifTrue = x, newNode)) ||
      ((ifFalse == oldNode) && util.applyOrFalse[EirExpressionNode](x => ifFalse = x, newNode))
  }
}

case class EirLiteral(var parent: Option[EirNode], var `type`: EirLiteralTypes.Value, var value: String)
  extends EirExpressionNode {

  def stripped: String = {
    `type` match {
      case EirLiteralTypes.String if value == "\"[]\"" => "[]"
      case EirLiteralTypes.String if value.matches("^\"[:a-zA-Z0-9_\\.\\/]+\"$") =>
        value.substring(1, value.length - 1)
      case _ => throw new RuntimeException(s"cannot strip $this")
    }
  }

  def toInt: Int = value.toInt

  def toBoolean: Boolean = value.toBoolean

  def equivalentTo(other: EirLiteral): Boolean = {
    `type` == other.`type` && value == other.value
  }

  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
}

object EirLiteralTypes extends Enumeration {
  type EirLiteralTypes = Value
  val String: Value = Value("string")
  val Integer: Value = Value("int")
  val Float: Value = Value("float")
  val Character: Value = Value("char")
  val Unit: Value = Value("unit")
  val Boolean: Value = Value("bool")
}

case class EirSymbol[T <: EirNamedNode : ClassTag](var parent: Option[EirNode], var qualifiedName: List[String])
  extends EirExpressionNode with EirResolvable[T] {

  private var _resolved : Seq[T] = Nil

  override def children: Iterable[EirNode] = Nil

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false

  override def resolve(): Seq[T] = {
    if (_resolved.isEmpty) {
      _resolved = Find.fromSymbol[T](this).toSeq
    }
    _resolved
  }

  def candidates: Seq[T] = resolve()

  override def resolved: Boolean = _resolved.nonEmpty
}

trait EirPostfixExpression extends EirExpressionNode {
  var target: EirExpressionNode
  var args: List[EirExpressionNode]

  override def children: Iterable[EirNode] = target +: args

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    AstManipulation.updateWithin(args, oldNode, newNode).map(args = _).isDefined ||
      ((target == oldNode) && util.applyOrFalse[EirExpressionNode](target = _, newNode))
  }
}

case class EirAwait(var parent: Option[EirNode], var target: EirExpressionNode) extends EirExpressionNode {
  var release: Option[EirExpressionNode] = None

  override def children: Iterable[EirNode] = List(target)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (target == oldNode) && util.applyOrFalse[EirExpressionNode](target = _, newNode)
  }
}

case class EirFunctionCall(var parent: Option[EirNode], var target: EirExpressionNode,
                           var args: List[EirExpressionNode], var types: List[EirResolvable[EirType]])
  extends EirPostfixExpression with EirSpecialization {
  override def children: Iterable[EirNode] = super.children ++ types
}

case class EirArrayReference(var parent: Option[EirNode], var target: EirExpressionNode, var args: List[EirExpressionNode])
  extends EirPostfixExpression {
}

case class EirScopedSymbol[T <: EirNode](var target: EirExpressionNode, var pending: EirResolvable[T])(var parent: Option[EirNode])
  extends EirExpressionNode with EirResolvable[T] {
  var isStatic = false
  override def resolve(): Seq[T] = Nil
  override def resolved: Boolean = pending.resolved
  override def children: Iterable[EirNode] = Seq(target)
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
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
  override def children: Iterable[EirNode] = declarations :+ expression

  var _declarations: List[EirDeclaration] = {
    identifiers.map(x => {
      val d = EirDeclaration(parent, isFinal = true, x, null, None)
      d.declaredType = EirPlaceholder(Some(d))
      d
    })
  }

  def declarations: List[EirDeclaration] = _declarations

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (expression == oldNode) && util.applyOrFalse[EirExpressionNode](expression = _, newNode)
  }
}

case class EirWhileLoop(var parent: Option[EirNode], var condition: Option[EirExpressionNode], var body: EirBlock) extends EirNode {
  override def children: Iterable[EirNode] = condition ++ List(body)
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirForLoop(var parent: Option[EirNode], var header: EirForLoopHeader, var body: EirBlock) extends EirNode with EirScope {
  override def children: Iterable[EirNode] = header.children ++ List(body)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((body == oldNode) && util.applyOrFalse[EirBlock](body = _, newNode)) ||
      header.replaceChild(oldNode, newNode)
  }
}

case class EirSpecializedSymbol(var parent: Option[EirNode],
                                var base: EirResolvable[EirNamedNode],
                                var types: List[EirResolvable[EirType]])
  extends EirExpressionNode with EirResolvable[EirType] with EirSpecialization {
  override def children: Iterable[EirNode] = base +: types

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    AstManipulation.updateWithin(types, oldNode, newNode).map(types = _).isDefined ||
      ((base == oldNode) && util.applyOrFalse[EirResolvable[EirNamedNode]](base = _, newNode))
  }

  override def resolve(): Seq[EirType] = Nil

  override def resolved: Boolean = false
}

case class EirIfElse(var parent: Option[EirNode], var test: EirExpressionNode,
                     var ifTrue: Option[EirNode], var ifFalse: Option[EirNode]) extends EirNode {
  override def children: Iterable[EirNode] = Seq(test) ++ ifTrue ++ ifFalse

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    ((test == oldNode) && util.applyOrFalse[EirExpressionNode](test = _, newNode)) ||
      (ifTrue.contains(oldNode) && util.applyOrFalse[EirNode](x => ifTrue = Some(x), newNode)) ||
      (ifFalse.contains(oldNode) && util.applyOrFalse[EirNode](x => ifFalse = Some(x), newNode))
  }
}

case class EirNew(var parent: Option[EirNode], var target: EirResolvable[EirType],
                  var args: List[EirExpressionNode]) extends EirExpressionNode {
  override def children: Iterable[EirNode] = target +: args

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirMatch(var parent: Option[EirNode], var expression: EirExpressionNode, var cases: List[EirMatchCase])
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = expression +: cases

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirMatchCase(var parent: Option[EirNode], var patterns: EirPatternList,
                        var condition: Option[EirExpressionNode], var body: Option[EirExpressionNode]) extends EirNode with EirScope {
  def declarations: List[EirDeclaration] = patterns.declarations
  override def children: Iterable[EirNode] = declarations ++ condition ++ body
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

trait EirPattern extends EirNode {
  def declarations: List[EirDeclaration]
  def conditions: List[EirExpressionNode]
  override def children: Iterable[EirNode] = declarations ++ conditions
  def position: Int = parent.to[EirPatternList].map(_.patterns.indexOf(this)).getOrElse(-1)
}

case class EirPatternList(var parent: Option[EirNode], var patterns: List[EirPattern]) extends EirPattern {
  override def declarations: List[EirDeclaration] = patterns.flatMap(_.declarations)
  override def conditions: List[EirExpressionNode] = patterns.flatMap(_.conditions)
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirExpressionPattern(var parent: Option[EirNode], var expression: EirExpressionNode) extends EirPattern with EirScope {
  val decl : EirDeclaration = EirDeclaration(Some(this), isFinal = true, "_", EirPlaceholder(None, None), None)
  override def declarations: List[EirDeclaration] = Nil
  override def conditions: List[EirExpressionNode] = List(expression)
  override def children: Iterable[EirNode] = super.children ++ List(decl)
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirIdentifierPattern(var parent: Option[EirNode], var name: String, private var _ty: EirResolvable[EirType]) extends EirPattern {
  private val declaration = EirDeclaration(Some(this), isFinal = true, name, _ty, None)
  if (_ty != null) _ty.parent = Some(declaration)

  def ty: EirResolvable[EirType] = _ty
  def ty_=(ty: EirResolvable[EirType]): Unit = {
    declaration.declaredType = ty
    _ty = ty
    _ty.parent = Some(declaration)
  }

  // TODO make context-specific
  var needsCasting = false

  override def declarations: List[EirDeclaration] = Option.when(name != "_")(declaration).toList
  override def conditions: List[EirExpressionNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    (ty == oldNode) && util.applyOrFalse[EirResolvable[EirType]](ty = _, newNode)
  }
}

case class EirInterpolatedString(var children: List[EirExpressionNode])(var parent: Option[EirNode]) extends EirExpressionNode {
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = {
    AstManipulation.updateWithin(children, oldNode, newNode).map(children = _).isDefined
  }
  def append(s: String): Unit = append(EirLiteral(Some(this), EirLiteralTypes.String, s))
  def append(n: EirExpressionNode): Unit = children :+= n
}

case class EirPackExpansion(var fqn: List[String])(var parent: Option[EirNode]) extends EirResolvable[EirType] {
  private var _resolved: Option[EirTemplateArgument] = None

  override def resolve(): Seq[EirType] = {
    if (_resolved.isEmpty) {
      val symbol = EirSymbol[EirNamedNode with EirType](parent, fqn)
      _resolved = symbol.resolve().headOption.to[EirTemplateArgument]
      if (!_resolved.forall(_.isPack)) {
        Errors.expectedParameterPack(this)
      }
    }
    _resolved.toSeq
  }

  override def resolved: Boolean = _resolved.isDefined
  override def children: Iterable[EirNode] = Nil
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = false
}

case class EirTypeAlias(var name: String, var templateArgs: List[EirTemplateArgument],
                        var value: EirResolvable[EirType])(var parent: Option[EirNode]) extends EirNamedNode with EirType with EirSpecializable {
  override def children: Iterable[EirNode] = templateArgs :+ value

  override def resolved: Boolean = value.resolved

  override def resolve(): List[EirType] = {
    if (templateArgs.nonEmpty) {
      ???
    } else {
      value.resolve().toList
    }
  }

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirConstantFacade(var value: EirLiteral)(var parent: Option[EirNode]) extends EirType {
  override def children: Iterable[EirNode] = List(value)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???

  override def equals(obj: Any): Boolean = {
    obj match {
      case EirConstantFacade(other) => value.equivalentTo(other)
      case _ => false
    }
  }
}

case class EirSdagWhen(var patterns: List[(EirSymbol[EirNamedNode], EirPatternList)],
                       var condition: Option[EirExpressionNode], var body: EirBlock)(var parent: Option[EirNode])
  extends EirNode with EirScope {
  override def children: Iterable[EirNode] = patterns.map(_._1) ++ declarations ++ condition :+ body

  def declarations: List[EirDeclaration] = patterns.flatMap(_._2.declarations)

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirSlice(var start: Option[EirExpressionNode], var step: Option[EirExpressionNode], var end: Option[EirExpressionNode])(var parent: Option[EirNode]) extends EirExpressionNode {
  override def children: Iterable[EirNode] = start ++ step ++ end

  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

case class EirAwaitMany(var waitAll: Boolean, var children: List[EirSdagWhen])(var parent: Option[EirNode]) extends EirNode {
  override def replaceChild(oldNode: EirNode, newNode: EirNode): Boolean = ???
}

//case class EirTypeOf(var parent: Option[EirNode], var exprNode: EirExpressionNode) extends EirExpressionNode {
//  override def eirType: EirResolvable[EirType] = exprNode.eirType
//
//  override def children: Iterable[EirNode] = List(exprNode)
//}