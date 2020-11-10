package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.types
import edu.illinois.cs.ergoline.types.EirType
import edu.illinois.cs.ergoline.util.EirResolvable

import scala.collection.mutable

object EirAccessibility extends Enumeration {
  type EirAccessibility = Value
  val Public, Private, Protected = Value
}

abstract class EirNode {
  var parent: Option[EirNode]
  var annotations: List[EirAnnotation] = Nil

  def validate(): Boolean
}

abstract class EirExpressionNode extends EirNode {
  def children: Iterable[EirNode]

  def eirType: EirResolvable[EirType]

  def toString: String
}

abstract class EirScope extends EirNode with EirScopedNode {
  var cachedSymbols: Option[(Int, Map[String, EirNode])] = None

  def children: Iterable[EirNode]

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

  override def validate(): Boolean = {
    val declarations = children.collect { case x: EirNamedNode => x.name }.toList
    declarations.distinct.size == declarations.size
  }
}

trait EirNamedNode extends EirNode {
  def name: String

  def fullyQualifiedName: List[String] = parent match {
    case Some(x: EirNamedNode) => x.fullyQualifiedName ++ List(name)
    case _ => List(name)
  }

  override def hashCode(): Int = name.hashCode
}

trait EirScopedNode extends EirNode {
  def scope: Option[EirScope] = {
    parent match {
      case Some(x: EirScope) => Some(x)
      case Some(x: EirScopedNode) => x.scope
      case _ => None
    }
  }
}

case class EirBlock(var parent: Option[EirNode], var children: Iterable[EirNode]) extends EirScope

case object EirGlobalNamespace extends EirScope {
  private val modules: mutable.HashMap[String, EirNamespace] = new mutable.HashMap

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
                          var declaredType: types.Allowed, var initialValue: Option[EirExpressionNode])
  extends EirNode with EirNamedNode with EirScopedNode {

  override def validate(): Boolean = ???
}

trait EirInheritable[T <: EirType] extends EirScopedNode with EirType {
  var extendsThis: Option[EirResolvable[T]]
  var implementsThese: List[EirResolvable[EirTrait]]
}

case class EirTemplateArgument(var parent: Option[EirNode]) extends EirNode {
  override def validate(): Boolean = true
}

case class EirClass(var parent: Option[EirNode], var members: List[EirMember],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirClass]],
                    var implementsThese: List[EirResolvable[EirTrait]])
  extends EirScope with EirNamedNode with EirInheritable[EirClass] {
  override def children: List[EirNode] = members ++ templateArgs
}

case class EirTrait(var parent: Option[EirNode], var members: List[EirMember],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirTrait]],
                    var implementsThese: List[EirResolvable[EirTrait]])
  extends EirScope with EirNamedNode with EirInheritable[EirTrait] {
  override def children: List[EirNode] = members ++ templateArgs
}

case class EirMember(var parent: Option[EirNode], var member: EirNamedNode, var accessibility: EirAccessibility.Value)
  extends EirNamedNode {
  override def toString: String = s"${accessibility.toString.toLowerCase} $name"

  override def validate(): Boolean = true

  def isConstructor: Boolean = member.isInstanceOf[EirFunction] && parent.map(_.asInstanceOf[EirNamedNode]).exists(_.name == name)

  override def name: String = member.name
}

case class EirFunction(var parent: Option[EirNode], var body: Option[EirNode],
                       var name: String, var templateArgs: List[EirTemplateArgument],
                       var functionArgs: List[EirFunctionArgument])
  extends EirScope with EirNamedNode {
  override def children: Iterable[EirNode] = body.map(List(_)).getOrElse(Nil) ++ templateArgs ++ functionArgs
}

case class EirAnnotation(var parent: Option[EirNode], var name: String) extends EirNode {
  override def validate(): Boolean = ???
}

case class EirBinaryExpression(var parent: Option[EirNode], var lhs: EirExpressionNode, var op: String, var rhs: EirExpressionNode)
  extends EirExpressionNode {
  override def children: Iterable[EirNode] = List(lhs, rhs)

  override def eirType: EirResolvable[EirType] = ???

  override def validate(): Boolean = ???

  override def toString: String = s"EirBinaryExpression($lhs $op $rhs)"
}

case class EirFunctionArgument(var parent: Option[EirNode], var name: String,
                               var declaredType: types.Allowed, var isFinal: Boolean, var isSelfAssigning: Boolean)
  extends EirNamedNode {
  override def validate(): Boolean = ???
}