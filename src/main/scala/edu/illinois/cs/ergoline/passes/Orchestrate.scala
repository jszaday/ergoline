package edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.ast.types.EirTemplatedType
import edu.illinois.cs.ergoline.ast._
import edu.illinois.cs.ergoline.globals
import edu.illinois.cs.ergoline.passes.Orchestrate.visit
import edu.illinois.cs.ergoline.passes.Pass.Phase
import edu.illinois.cs.ergoline.resolution.{Find, Modules}
import edu.illinois.cs.ergoline.util.EirUtilitySyntax.RichOption
import edu.illinois.cs.ergoline.util.{
  AstManipulation,
  Errors,
  TypeCompatibility
}

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
  private val annotationName = "charisma"
  private def namespace: Option[EirScope] = Modules("or", EirGlobalNamespace)
  private def placeholder: EirClass =
    Find.namedChild[EirClass](namespace, "placeholder")

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
      case Some(x: EirAssignment)                 => x.lval == node
      case Some(x) if !x.isInstanceOf[EirForLoop] => isProducer(x)
      case _                                      => false
    }
  }

  @tailrec
  def visit(node: EirNode)(implicit ctx: TypeCheckContext): Unit = {
    node match {
      case x: EirMember   => visit(x.member)(ctx)
      case f: EirFunction => visit(f)(ctx)
    }
  }

  def visit(fn: EirFunction)(implicit ctx: TypeCheckContext): Unit = {
    val pred = (x: EirNode) => x.annotations.find(_.name == annotationName)
    val annotation = pred(fn).orElse(fn.parent.to[EirMember].flatMap(pred))
    assert(annotation.nonEmpty)
    annotation.foreach(_.name = "threaded")

    var loops = Set[EirForLoop]()
    val body = fn.body.getOrElse(Errors.unreachable())

    // TODO ( use a recursive-descent search algo. here )
    val decls = Find
      .child[EirDeclaration](
        body,
        (d: EirDeclaration) => {
          val declTy = CheckTypes.visit(d.declaredType)
          TypeCompatibility.isSpecializationOf(placeholder, declTy)
        }
      )
      .map(d => {
        val declTy =
          CheckTypes.visit(d.declaredType).asInstanceOf[EirTemplatedType]
        (d, declTy.args.head)
      })
      .toMap

    val uses = Find
      .descendant(
        body,
        {
          // TODO ( fix context info so we don't have to rely on cached type )
          case x: EirArrayReference => x.target.foundType.map(ty => {
              TypeCompatibility.isSpecializationOf(placeholder, ty)
            })
          case _: EirImport => None
          case _            => Some(false)
        }
      )
      .map(_.asInstanceOf[EirArrayReference])
      .toList

    val pubs = new mutable.ListBuffer[EirArrayReference]

    // TODO ( check all self[@] refers to self )

    for (use <- uses) {
      val loop = use.args.headOption.flatMap(forRelated)
      if (use.args.length != 1 || loop.isEmpty)
        Errors.exit(s"unsure how to use $use")
      val target = use.target.disambiguation.to[EirDeclaration]
      val argTy = target.flatMap(decls.get)
      val name = target.map(_.name)
      val arg = use.args.head

      arg match {
        case _: EirSymbolLike[_] =>
          if (isProducer(use)) {
            pubs.prepend(use)

            use.parent.foreach(_.replaceChild(use, use.target))
          } else {
            val pubIdx = pubs.indexWhere(
              _.target.disambiguation == use.target.disambiguation
            )
            if (pubIdx < 0) {
              Errors.exit(s"consumer with no producer, $use")
            }
            val pub = pubs(pubIdx)
            val isSymbol =
              pub.args.headOption.exists(_.isInstanceOf[EirSymbol[_]])

            if (!isSymbol) {
              AstManipulation.insertBefore(
                use, {
                  val node = GenerateCpp.CppNode({
                    val cgen = new CodeGenerationContext("cpp", ctx)
                    cgen << "auto" << name.map(_ + "_req") <<
                      "=std::make_shared<hypercomm::resuming_callback<" << argTy
                        .map(
                          cgen.typeFor(_)
                        ) << ">>();"
                    cgen << "this->open(" << name.map(
                      _ + "_port"
                    ) << "," << name.map(_ + "_req") << ");"
                    cgen << name.map(_ + "_req") << "->wait();"
                    cgen.toString()
                  })
                  node.foundType = use.foundType
                  node
                }
              )
            }

            use.parent.foreach(
              _.replaceChild(
                use,
                pub.args.head match {
                  case _: EirSymbolLike[_] => use.target
                  case _ =>
                    val node = GenerateCpp.CppNode(
                      name.get + "_req->value()"
                    )
                    node.foundType = use.foundType
                    node
                }
              )
            )
          }
        case _ =>
          if (isProducer(use)) {
            pubs.prepend(use)

            AstManipulation.insertBefore(
              use, {
                val node = GenerateCpp.CppNode({
                  val cgen = new CodeGenerationContext("cpp", ctx)
                  cgen << "auto" << name.map(
                    _ + "_port"
                  ) << "=" << "std::make_shared<hypercomm::temporary_port<std::string>>(std::string(\"" << name << "\"));"
                  cgen.toString()
                })
                node.foundType = use.foundType
                node
              }
            )

            GenerateCpp.corePupables += "hypercomm::temporary_port<std::string>"

            val assign = Find.ancestors(use) collectFirst {
              case a: EirAssignment => a
            }
            assign.flatMap(_.parent).zip(assign) foreach {
              case (parent, assign) => parent.replaceChild(
                  assign, {
                    implicit val visitor
                        : (CodeGenerationContext, EirNode) => Unit =
                      GenerateCpp.visitor
                    val cgen = new CodeGenerationContext("cpp", ctx)
                    cgen.pushSelf("this->impl_")
                    cgen << "hypercomm::send2port(this->thisProxy[hypercomm::conv2idx<CkArrayIndex>(" << arg << ")]," << name
                      .map(
                        _ + "_port"
                      ) << ",hypercomm::make_typed_value<" << argTy
                      .map(
                        cgen.typeFor(_)
                      ) << ">(" << assign.rval << "));"
                    GenerateCpp.CppNode(cgen.toString())
                  }
                )
            }
          } else {
            Errors.exit("consumers with offsets unsupported")
          }
      }
      loops ++= loop
    }

    decls foreach { case (d, argTy) =>
      val repl = EirDeclaration(
        d.parent,
        isFinal = false,
        d.name,
        argTy,
        None
      )
      body.replaceChild(d, repl)
    }

    loops.foreach(loop => {
      val hdrDecls = loop.header.declaration.to[EirDeclaration].toList
      if (hdrDecls.isEmpty) {
        Errors.exit(
          s"unsure how to declare [ ${loop.header.declaration mkString ", "} ]"
        )
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
      val block = EirBlock(loop.parent, replDecls ++ loop.body)
      loop.parent.foreach(_.replaceChild(loop, block))
    })
  }
}
