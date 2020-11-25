package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirGlobalNamespace, EirNode}
import edu.illinois.cs.ergoline.resolution.Find

object Processes {
  def onLoad(x : EirNode): Unit = {
    FullyResolve.visit(x)
    Find.all[EirClassLike](x).foreach(CheckClasses.visit)
    CheckTypes.visit(new TypeCheckContext, x)
  }

  def generateCpp(): Iterable[String] = {
    EirGlobalNamespace.children.map(GenerateCpp.visit)
  }
}