package edu.illinois.cs.ergoline.ast

import edu.illinois.cs.ergoline.ast

import scala.collection.mutable

abstract class EirNode {
  var parent: Option[EirNode]

  def validate() : Boolean
}

class EirResolvable[T](qualifiers: List[String], name: String) {
  def resolve(scope: EirScope): Option[T] = {
    var qualified = scope
    for (qualifier <- qualifiers) {
      qualified = scope(qualifier) match {
        case Some(x: EirScope) => x
        case _ => return None
      }
    }
    qualified(name) match {
      case Some(x: T) => Some(x)
      case _ => None
    }
  }
}

object EirImplicits {
  implicit def identListToResolvable[T](value: List[String]): EirResolvable[T] = {
    if (value.isEmpty) throw new RuntimeException("expected at least one value in list")
    else new EirResolvable[T](value.init, value.last)
  }
}

trait EirType {
  var name: String

  def canCastTo(other: EirType): Boolean
}

trait EirExpression {
  def eirType(): EirType
}

abstract class EirScope extends EirNode with EirScopedNode {
  var cachedSymbols: Option[(Int, Map[String, EirNode])] = None
  var children: List[EirNode]

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
    val declarations = children.collect { case x: EirNamedNode => x.name }
    declarations.distinct.size == declarations.size
  }
}

trait EirNamedNode extends EirNode {
  var name: String

  def fullyQualifiedName: List[String] = parent match {
    case Some(x: EirNamedNode) => x.fullyQualifiedName ++ List(name)
    case _ => List(name)
  }
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

case object EirGlobalNamespace extends EirScope {
  def put(name : String, eirNamespace: EirNamespace): Option[EirNamespace] = modules.put(name, eirNamespace)

  private val modules : mutable.HashMap[String, EirNamespace] = new mutable.HashMap
  override def symbols: Map[String, EirNode] = modules.toMap
  override var children: List[EirNode] = List.empty
  override var parent: Option[EirNode] = None
}

case class EirNamespace(var parent: Option[EirNode], var children: List[EirNode], var name: String)
  extends EirScope with EirNamedNode {

}

case class EirDeclaration(var parent: Option[EirNode], var isFinal: Boolean, var name: String,
                          var declaredType: EirResolvable[EirType], var initialValue: Option[EirExpression])
  extends EirNode with EirNamedNode with EirScopedNode {

  override def validate(): Boolean = {
    if (isFinal && initialValue.isEmpty) false
    else initialValue.map(_.eirType().canCastTo(declaredType.resolve(scope.get).get)).getOrElse(true)
  }
}

trait EirInheritable[T <: EirType] extends EirScopedNode with EirType {
  var extendsThis: Option[EirResolvable[T]]
  var implementsThese: List[EirResolvable[EirTrait]]

  def canCastTo(other: EirType): Boolean =
    extendsThis.exists(_.resolve(scope.get).exists(_.canCastTo(other))) ||
      implementsThese.exists(_.resolve(scope.get).exists(_.canCastTo(other))) ||
      // TODO this last check may, eventually, need to be more sophisticated
      other == this
}

case class EirTemplateArgument(var parent: Option[EirNode]) extends EirNode {
  override def validate(): Boolean = true
}

case class EirClass(var parent: Option[EirNode], var children: List[EirNode],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirClass]],
                    var implementsThese: List[EirResolvable[EirTrait]])
  extends EirScope with EirNamedNode with EirInheritable[EirClass] {

}

case class EirTrait(var parent: Option[EirNode], var children: List[EirNode],
                    var name: String, var templateArgs: List[EirTemplateArgument],
                    var extendsThis: Option[EirResolvable[EirTrait]],
                    var implementsThese: List[EirResolvable[EirTrait]])
  extends EirScope with EirNamedNode with EirInheritable[EirTrait] {

}