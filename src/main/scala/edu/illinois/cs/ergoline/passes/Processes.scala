package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirGlobalNamespace, EirNode}

object Processes {
  def onLoad(x : EirNode): Unit = {
    FullyResolve.visit(x)
    CheckTypes.visit(new TypeCheckContext, x)
  }

  def generateCpp(): Iterable[String] = {
    EirGlobalNamespace.children.map(GenerateCpp.visit)
  }
}
