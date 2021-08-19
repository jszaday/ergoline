package edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast.{EirArrayReference, EirAssignment, EirBinaryExpression, EirBlock, EirClass, EirDeclaration, EirExpressionNode, EirForLoop, EirFunction, EirGlobalNamespace, EirImport, EirMember, EirNamedNode, EirNode, EirScopedSymbol, EirSymbol, EirSymbolLike, EirVisitor}
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.Orchestrate.visit
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{Errors, TypeCompatibility}

import scala.annotation.tailrec
import scala.collection.mutable

class Orchestrate extends Pass {
  override def phase: Phase = Phase.Optimize

  override def apply(n: EirNode): Unit = {
    visit(n)(Processes.typeContext())
  }

  override def after: Seq[Pass] = Seq()

  override def annotations: Seq[String] = Seq(Orchestrate.annotationName)
}

object Orchestrate {
  val annotationName = "charisma"
  val namespace = Modules("or", EirGlobalNamespace)
  val placeholder = Find.namedChild[EirClass](namespace, "placeholder")

  def forRelated(node: EirNode): Option[EirForLoop] = {
    node match {
      case s: EirSymbolLike[_] => Find
          .uniqueResolution[EirNode](s)
          .parent
          .filter(_.isInstanceOf[EirForLoop])
          .to[EirForLoop]
      case x: EirBinaryExpression => forRelated(x.lhs).orElse(forRelated(x.rhs))
      case _                      => None
    }
  }

  def selfIndexAt(i: Int): EirExpressionNode = {
    val s = GenerateCpp.CppNode(s"((this->thisIndexMax).data())[$i]")
    s.foundType = Some(globals.integerType)
    s
  }

  @tailrec
  def isProducer(node: EirNode): Boolean = {
    node.parent match {
      case Some(x: EirAssignment) => x.lval == node
      case Some(x) if !x.isInstanceOf[EirForLoop] => isProducer(x)
      case _ => false
    }
  }

  def visit(node: EirNode)(implicit ctx: TypeCheckContext): Unit = {
    node match {
      case x: EirMember => {
        visit(x.member)(ctx)
      }
      case f: EirFunction => visit(f)(ctx)
    }
  }

  def visit(fn: EirFunction)(implicit ctx: TypeCheckContext): Unit = {
    val pred = (x: EirNode) => x.annotations.exists(_.name == annotationName)
    assert(pred(fn) || fn.parent.to[EirMember].exists(pred))
    var loops = Set[EirForLoop]()
    val body = fn.body.getOrElse(???)

    // TODO ( use a recursive-descent search algo. here )
    val decls = Find
      .child[EirDeclaration](
        body,
        (d: EirDeclaration) => {
          val declTy = CheckTypes.visit(d.declaredType)
          TypeCompatibility.isSpecializationOf(placeholder, declTy)
        }
      )
      .toList

    val uses = Find
      .descendant(
        body,
        {
          // TODO ( fix context info so we don't have to rely on cached type )
          case x: EirArrayReference => x.target.foundType.map(ty => {
              TypeCompatibility.isSpecializationOf(placeholder, ty)
            })
          case x: EirImport => None
          case _            => Some(false)
        }
      )
      .map(_.asInstanceOf[EirArrayReference])
      .toList

    println("decls: " + decls)
    println("uses: " + uses)

    val pubs = new mutable.ListBuffer[EirArrayReference]

    for (use <- uses) {
      val loop = use.args.headOption.flatMap(forRelated)
      if (use.args.length != 1 || loop.isEmpty)
        Errors.exit(s"unsure how to use $use")
      val arg = use.args.head
      arg match {
        case _: EirSymbolLike[_] =>
          if (isProducer(use)) pubs.prepend(use)
          use.parent.foreach(_.replaceChild(use, use.target))
        case _ =>
          if (isProducer(use)) {
            Errors.exit("producer with iterator offset")
          }
          val pubIdx = pubs.indexWhere(_.target.disambiguation == use.target.disambiguation)
          if (pubIdx < 0) {
            Errors.exit(s"consumer with no producer, $use")
          }
          val pub = pubs(pubIdx)
          println(s"todo: pairing $pub with $use")
      }
      loops ++= loop
    }

    decls.foreach(d => {
      val declTy =
        CheckTypes.visit(d.declaredType).asInstanceOf[EirTemplatedType]
      val repl = EirDeclaration(
        d.parent,
        isFinal = false,
        d.name,
        declTy.args.head,
        None
      )
      body.replaceChild(d, repl)
    })

    loops.foreach(loop => {
      val hdrDecls = loop.header.declarations
      if (hdrDecls.length != 1) {
        Errors.exit(s"unsure how to declare [ ${hdrDecls mkString ", "} ]")
      }
      val replDecls = hdrDecls.zipWithIndex.map({ case (d, i) =>
        EirDeclaration(
          None,
          isFinal = true,
          d.name,
          d.declaredType,
          Some(selfIndexAt(i))
        )
      })
      // TODO ( assert idx is in range of for-loop )
      val block = EirBlock(loop.parent, replDecls :+ loop.body)
      loop.parent.foreach(_.replaceChild(loop, block))
    })
  }
}
