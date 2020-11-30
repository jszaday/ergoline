package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirGlobalNamespace, EirNode}
import edu.illinois.cs.ergoline.proxies.ProxyManager
import edu.illinois.cs.ergoline.resolution.Find
import scala.util.Properties.{lineSeparator => n}

object Processes {
  def onLoad(x : EirNode): Unit = {
    FullyResolve.visit(x)
    Find.all[EirClassLike](x).foreach(CheckClasses.visit)
    CheckTypes.visit(new TypeCheckContext, x)
  }

  def generateCpp(): Iterable[String] = {
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    a.map(GenerateCpp.forwardDecl(_)) ++ Seq("#include \"generate.decl.h\"") ++
    a.map(GenerateCpp.visit) ++
    EirGlobalNamespace.children.filterNot(_.name == "ergoline").map(GenerateCpp.visit) ++
    c.map(GenerateCpp.visit) ++  Seq(n + "#include \"generate.def.h\"")
  }
}
