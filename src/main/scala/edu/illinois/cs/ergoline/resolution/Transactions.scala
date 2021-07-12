package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirSpecializable, EirSpecialization, EirTemplateArgument}
import edu.illinois.cs.ergoline.ast.types.EirType

import scala.collection.mutable
import scala.reflect.{ClassTag, classTag}

object Transactions {

  abstract class EirTransaction {
    var active: Boolean = false

    def deactivate(): Unit = { active = false }
  }

  case class EirSpecializeTransaction(
     var pair: (EirSpecializable, EirSpecialization),
     var args: List[EirTemplateArgument],
     var types: List[EirResolvable[EirType]]
  ) extends EirTransaction {
    def to[A : ClassTag]: A = {
      classTag[A] match {
        case t if t == classTag[EirSpecializable] => pair._1.asInstanceOf[A]
        case t if t == classTag[EirSpecialization] => pair._2.asInstanceOf[A]
        case _ => ???
      }
    }
  }

  object EirSpecializeTransaction {
    def apply(
      target: (EirSpecializable, EirSpecialization),
      zipped: List[(EirTemplateArgument, EirResolvable[EirType])]
    ): EirSpecializeTransaction = { EirSpecializeTransaction(target, zipped.map(_._1), zipped.map(_._2)) }
  }

  abstract class Manager[A <: EirTransaction] {
    private val _transactions: mutable.ListBuffer[A] = new mutable.ListBuffer

    def transactions: Iterable[A] = _transactions.filter(_.active)

    def allTransactions: Iterable[A] = _transactions

    def activate(a: A): A = {
      _transactions.append(a)
      a.active = true
      a
    }
  }
}
