package edu.illinois.cs.ergoline.passes
import edu.illinois.cs.ergoline.ast.EirNode
import edu.illinois.cs.ergoline.passes.Pass.Phase

class Orchestrate extends Pass {
  override def phase: Phase = Phase.Optimize

  override def apply(n: EirNode): Unit = {
    println(s"here with ${n}")
  }

  override def after: Seq[Pass] = Seq()

  override def annotations: Seq[String] = Seq("charisma")
}
