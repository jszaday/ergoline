package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.resolution.EirResolvable
import org.antlr.v4.runtime.ParserRuleContext

import scala.jdk.CollectionConverters.ListHasAsScala

package object util {

  private def emplaceNamespace(parent : Option[EirNode], name : String): EirNamespace = {
    val ns = EirNamespace(parent, Nil, name)
    parent match {
      case Some(EirGlobalNamespace) =>
        EirGlobalNamespace.put(ns.name, ns)
      case Some(x: EirNamespace) =>
        x.children ++= List(ns)
      case _ => throw new RuntimeException("unacceptable module target")
    }
    ns
  }

  def createOrFindNamespace(parent: Option[EirNode], fqn: Iterable[String]): EirNamespace =
    createOrFindNamespace(parent, fqn.toList)

  def createOrFindNamespace(parent: Option[EirNode], fqn: List[String]): EirNamespace = {
    fqn match {
      case head :: Nil  => parent.flatMap(_.scope.flatMap(_.find(head))).getOrElse(emplaceNamespace(parent, head))
      case init :+ last =>
        val ns: Option[EirNamespace] = Some(createOrFindNamespace(parent, init))
        createOrFindNamespace(ns, List(last))
    }
  }

  def encloseNodes(nodes: EirNode*): EirBlock = {
    val b = EirBlock(nodes.head.parent, nodes.toList)
    nodes.foreach(_.parent = Some(b))
    b
  }

  def makeTupleElementGetter(expression: EirExpressionNode, idx: Int): EirExpressionNode = {
    val ref = EirArrayReference(expression.parent, expression, null)
    expression.parent = Some(ref)
    ref.args = List(EirLiteral(Some(ref), EirLiteralTypes.Integer, idx.toString))
    ref
  }

  def visitAll[T](node : EirNode, f : EirNode => T): Seq[T] = {
    f(node) +: node.children.flatMap(n => {
      f(n) +: visitAll(n, f)
    }).toSeq
  }

  // TODO implement this
  def deepCloneTree[T <: EirNode](node : T): T = node

  object EirUtilitySyntax {

    implicit class RichOption(option: Option[_]) {
      def to[T: Manifest]: Option[T] = option match {
        case Some(t: T) => Some(t)
        case _ => None
      }
    }

    implicit class RichResolvableTypeIterable(types: Iterable[EirResolvable[EirType]]) {
      def toTupleType(implicit parent : Option[EirNode]): EirResolvable[EirType] =
        types.toList match {
          case Nil => throw new RuntimeException("please use unit type")
          case element :: Nil => element
          case x => EirTupleType(parent, x)
        }
    }

    implicit class RichParserRuleContext[T <: ParserRuleContext](t: T) {
      def mapOrEmpty[A, B](f: T => java.util.List[A], g: A => B): List[B] =
        Option(t).map(f).map(_.asScala).getOrElse(Nil).map(g).toList
    }

  }

}
