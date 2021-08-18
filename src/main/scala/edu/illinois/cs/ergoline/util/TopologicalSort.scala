package edu.illinois.cs.ergoline.util

object TopologicalSort {
  trait Constrained[A] {
    def after: Seq[A]
  }

  /* TODO do this more efficiently with a topological sorting algo
   *      instead of greedy sorting algorithm
   */
  def sort[A <: Constrained[_]](l: List[A]): List[A] = {
    var placed: List[A] = Nil
    var unplaced = l.sortBy(_.after.size)
    while (unplaced.nonEmpty) {
      val idx = unplaced.indexWhere(
        !_.after.exists(unplaced.contains(_))
      )
      placed :+= unplaced(idx)
      unplaced = unplaced.patch(idx, Nil, 1)
    }
    placed
  }
}
