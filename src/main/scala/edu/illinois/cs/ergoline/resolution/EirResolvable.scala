package edu.illinois.cs.ergoline.resolution

import edu.illinois.cs.ergoline.ast.{EirNode, EirScope}

trait EirResolvable[T] {
  def resolve(scope: EirScope): T
  def represents: Option[EirNode]
}
