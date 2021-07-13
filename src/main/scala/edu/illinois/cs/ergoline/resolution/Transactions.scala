package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirSpecializable, EirSpecialization}

object Transactions {
  abstract class EirTransaction

  case class EirSubstituteTransaction(
      s: EirSpecializable,
      sp: EirSpecialization
  ) extends EirTransaction
}
