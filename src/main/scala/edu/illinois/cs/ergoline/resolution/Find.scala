package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.ast.types._
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichIntOption
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.{RichBoolean, RichEirNode, RichOption}
import edu.illinois.cs.ergoline.util.assertValid

import scala.collection.mutable
import scala.reflect.ClassTag

object Find {
  type EirNamedScope = EirScope with EirNamedNode

  def ancestors(x: EirNode): Seq[EirNode] = x.parent match {
    case Some(parent) => parent +: ancestors(parent)
    case None => Nil
  }

  // find where x's ancestors first overlap with y's
  def commonAncestor(x: EirNode, y: EirNode): Option[EirNode] = {
    val ancestors = Find.ancestors(x).toList
    Find.ancestors(y).find(ancestors.contains(_))
  }

  def withName[T <: EirNamedNode](name: String): T => Boolean =
    (n: T) => n.name == name

  def qualifications(scope: EirScope, fqn: List[String])(implicit ctx: Option[EirNode] = None): Iterable[EirNamedScope] = {
    fqn match {
      case head :: Nil => globally[EirNamedScope](scope, withName(head))
      case init :+ last => qualifications(scope, init)
        .flatMap(_.findChild[EirNamedScope](withName(last)))
      case _ => Nil
    }
  }

  // search up and down from a node
  def globally[T <: EirNode : Manifest](scope: EirScope, predicate: T => Boolean, searched: mutable.ArrayBuffer[EirScope] = mutable.ArrayBuffer())(implicit ctx: Option[EirNode] = None): Iterable[T] = {
    (ctx.forall(_.canAccess(scope)) && !searched.contains(scope)).ifTrue({
      searched += scope
      Find.within(scope, predicate) ++ scope.scope.toIterable.flatMap(globally(_, predicate, searched))
    })
  }

  def descendant(node: EirNode, predicate: EirNode => Option[Boolean]): Iterable[EirNode] = {
    node.children.zip(node.children.map(predicate)).collect({
      case (node, Some(x)) => Option.when(x)(node) ++ descendant(node, predicate)
    }).flatten
  }

  // recursively check all children of a node
  def within[T <: EirNode](node: EirNode, predicate: T => Boolean)(implicit tag: ClassTag[T]): Iterable[T] = {
    Find.child[T](node, predicate) ++ node.children.flatMap(within[T](_, predicate))
  }

  def matchesPredicate[T](predicate: T => Boolean)(x : Any): Boolean = {
    try {
      predicate(x.asInstanceOf[T])
    } catch {
      case _ : ClassCastException => false
    }
  }

  // check only the immediate children of the node (do not descend)
  def child[T <: EirNode](node: EirNode, predicate: T => Boolean)(implicit tag: ClassTag[T]): Iterable[T] = {
    node.children.filter(matchesPredicate(predicate)).map(_.asInstanceOf[T])
  }

  def all[T <: EirNode : Manifest](node: EirNode): Iterable[T] = {
    node.children.flatMap(all(_)) ++
      node.children.collect {
        case x: T => x
      }
  }

  def annotatedWith[T <: EirNode : Manifest](scope: EirScope, name: String): Iterable[T] = {
    Find.within[T](scope, _.annotations.exists(_.name == name))
  }

  // find values strictly "owned" by the owner, such that no other instance of its class owns it as well
//  def owned[C <: EirNode : Manifest, T <: EirNode : Manifest](owner : C): Iterable[T] = {
//    owner.findWithin[T]((t : T) => parentOf[C](t).contains(owner))
//  }

  def unionType(types: Iterable[EirType]): Option[EirType] = {
    types match {
      case Nil => None
      case head :: Nil => Some(head)
      case _ => ???
    }
  }

  def unionType(types: EirType*): Option[EirType] = unionType(types)

  def parentOf[T <: EirNode : Manifest](node: EirNode): Option[T] =
    node.parent.to[T].orElse(node.parent.flatMap(parentOf[T]))

  import FindSyntax.RichPredicate

  def isTopLevel(x : EirNode): Boolean = x match {
    case _ : EirBlock => true
    case _ : EirForLoop => true
    case _ : EirLambdaExpression => true
    case _ : EirFunction => true
    case _ : EirClassLike => true
    case _ : EirNamespace => true
    case _ : EirImport => true
    case _ => false
  }

  def anywhereAccessible(ctx : EirNode, name : String): Seq[EirNamedNode] = {
//    val name = symbol.qualifiedName.last
    val ancestors = Find.ancestors(ctx).filter(isTopLevel)
    val matches = matchesPredicate(withName(name))(_)
    val predicate: EirNode => Option[Boolean] = {
      case _ : EirBlock => None
      // only allowed to consider members when within the class?
      case x : EirMember => Option.when(x.parent.exists(ancestors.contains))(matches(x))
      case x : EirFunction => Option.when(matches(x))(true)
      case x : EirNamespace => Option.when(matches(x) || ancestors.contains(x))(matches(x))
      case x : EirClassLike => Option.when(!ancestors.contains(x) || matches(x))(matches(x))
      case x if x.parent.exists(_.isInstanceOf[EirMember]) => Some(false)
      case x if !isTopLevel(x) => Option.when(matches(x))(true)
      case x => Some(matches(x))
    }
    ancestors.flatMap(ancestor =>
      Find.descendant(ancestor, predicate).filter(x => {
        ancestor match {
          case block: EirBlock => block.findPositionOf(ctx) > block.findPositionOf(x)
          case _ => true
        }
      })
    ).filter(ctx.canAccess(_)).map({
      case fs : EirFileSymbol => fs.resolve().head
      case x => x
    }).map(_.asInstanceOf[EirNamedNode])
  }

  def fromSymbol[T <: EirNamedNode : ClassTag](symbol : EirSymbol[T]): Seq[T] = {
    symbol.qualifiedName match {
      case last :: Nil => anywhereAccessible(symbol, last).collect({
        case x: T => x
      })
      case init :+ last =>
        var namespace = anywhereAccessible(symbol, init.head)
        for (mid <- init.tail) {
          namespace = namespace.flatMap(child[EirNamedNode](_, withName(mid).and(symbol.canAccess)))
        }
        namespace.flatMap(child[T](_, withName(last).and(symbol.canAccess)))
    }
  }

  def singleReference[T <: EirNode](resolvable: EirResolvable[T]): Option[T] = {
    val found = resolvable.resolve()
    Option.when(found.length == 1)(found.head)
  }

  def uniqueResolution[T <: EirNode](x: EirResolvable[T]): T = {
    x.resolve() match {
      case head :: Nil => head
      case _ => throw new RuntimeException(s"unable to uniquely resolve $x")
    }
  }

  def uniqueResolution[T <: EirNode](iterable: Iterable[EirResolvable[T]]): Iterable[T] = {
    iterable.map(uniqueResolution[T])
  }

  def accessibleMember(base : EirNode, x : EirFieldAccessor): List[EirMember] = {
    val scope = base match {
      case EirTemplatedType(_, s : EirClassLike, _) => s
      case s : EirClassLike => s
      case _ => throw new RuntimeException(s"unsure how to find members of $base")
    }
    // TODO check parent classes as well!
    child[EirMember](scope, withName(x.field).and(x.canAccess(_))).toList
  }

  def callable(x : EirClassLike): List[EirMember] = {
    // TODO may need to check if first argument is self or not?
    child[EirMember](x, withName("apply").and(x.canAccess(_))).toList
  }

  object FindSyntax {

    implicit class RichPredicate[T](predicate: T => Boolean) {
      def and(other: T => Boolean): T => Boolean = (t: T) => predicate(t) && other(t)
    }

  }

}
