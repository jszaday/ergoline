package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.EirNode

trait EirResolvable[T] extends EirNode {
  def resolved: T
}
