package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{
  EirSpecializable,
  EirSpecialization,
  EirTemplateArgument
}
import edu.illinois.cs.ergoline.ast.types.EirType

import scala.collection.SeqOps
import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

object Transactions {

  abstract class EirTransaction {
    private var _active: Boolean = false

    def active: Boolean = _active
    def activate(): Unit = { _active = true }
    def deactivate(): Unit = { _active = false }
  }

  case class EirSpecializeTransaction(
      var pair: (EirSpecializable, EirSpecialization),
      var args: List[EirTemplateArgument],
      var types: List[EirResolvable[EirType]]
  ) extends EirTransaction {
    def to[A: ClassTag]: A = {
      classTag[A] match {
        case t if t == classTag[EirSpecializable]  => pair._1.asInstanceOf[A]
        case t if t == classTag[EirSpecialization] => pair._2.asInstanceOf[A]
        case _                                     => ???
      }
    }
  }

  object EirSpecializeTransaction {
    def apply(
        target: (EirSpecializable, EirSpecialization),
        zipped: List[(EirTemplateArgument, EirResolvable[EirType])]
    ): EirSpecializeTransaction = {
      EirSpecializeTransaction(target, zipped.map(_._1), zipped.map(_._2))
    }
  }

  abstract class Manager[A <: EirTransaction] {
    private val _transactions: ListBuffer[A] = new ListBuffer

    type SeqIterable = Iterable[A] with SeqOps[A, ListBuffer, ListBuffer[A]]

    def transactions: SeqIterable = _transactions.filter(_.active)

    def allTransactions: SeqIterable = _transactions

    def activate(a: A): A = {
      _transactions.append(a)
      a.activate()
      a
    }
  }
}
