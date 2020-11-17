package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode

trait EirResolvable[+T <: EirNode] extends EirNode {
  def resolve(): T
  def resolved: Boolean
}
