package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode

trait EirResolvable[+T <: EirNode] extends EirNode {
  def resolve(): List[T]
  def resolved: Boolean
}
