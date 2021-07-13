package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirSpecializable, EirSpecialization}

import scala.collection.mutable.ListBuffer

object Transactions {
  abstract class EirTransaction {
    private[this] var _active = false
    private[Transactions] def active: Boolean = _active
    private[Transactions] def activate(): Unit = _active = true
    private[Transactions] def deactivate(): Unit = _active = false
  }

  case class EirSubstituteTransaction(
      s: EirSpecializable,
      sp: EirSpecialization
  ) extends EirTransaction

  class Manager[A <: EirTransaction] {
    private[this] val _transactions: ListBuffer[A] = new ListBuffer[A]
    private[this] var _recording: Boolean = false

    def startRecording(): Unit = _recording = true
    def stopRecording(): Unit = _recording = false

    def activate(a: A): A = {
      _transactions.prepend(a)
      a.activate()
      a
    }

    def deactivate(a: A): Unit = {
      a.deactivate()

      if (!_recording) {
        _transactions.remove(_transactions.indexOf(a))
      }
    }

    private type Iterable = scala.collection.Iterable[A]
      with scala.collection.SeqOps[A, ListBuffer, ListBuffer[A]]

    def transactions: Iterable = _transactions.filter(_.active)
  }
}
