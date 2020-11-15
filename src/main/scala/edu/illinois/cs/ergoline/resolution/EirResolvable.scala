package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirNode, EirScope}

trait EirResolvable[T] extends EirNode {
  def resolve(scope: EirScope): T
}
