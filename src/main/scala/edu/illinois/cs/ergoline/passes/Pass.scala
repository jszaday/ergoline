package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.EirNode
import edu.illinois.cs.ergoline.util.TopologicalSort

import scala.reflect.ClassTag

object Pass {
  object Phase extends Enumeration {
    type Phase = Value
    val Load, Optimize, Compile = Value
  }
}

abstract class Pass extends TopologicalSort.Constrained[Pass] {
  type Phase = Pass.Phase.Value
  def phase: Phase
  def apply(n: EirNode): Unit
  def canEnter[B <: EirNode: ClassTag]: Boolean = false
  def annotations: Seq[String] = Seq()
}
