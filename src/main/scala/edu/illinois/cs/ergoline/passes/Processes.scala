package edu.illinois.cs.ergoline.passes

import edu.illinois.cs.ergoline.ast.{EirClassLike, EirFileSymbol, EirGlobalNamespace, EirNode}
import edu.illinois.cs.ergoline.proxies.ProxyManager
import edu.illinois.cs.ergoline.resolution.Find

import scala.util.Properties.{lineSeparator => n}

object Processes {
  def onLoad(x : EirNode): Unit = {
    FullyResolve.visit(x)
    Find.all[EirClassLike](x).foreach(CheckClasses.visit)
    CheckTypes.visit(new TypeCheckContext, x.children.filter({
      case fs : EirFileSymbol => fs.resolved
      case _ => true
    }))
  }

  def generateCpp(): Iterable[String] = {
    val (a, c) = ProxyManager.proxies.toList.partition(_.isAbstract)
    val body = a.map(GenerateCpp.visit) ++
      EirGlobalNamespace.children.filterNot(_.name == "ergoline").map(GenerateCpp.visit) ++ c.map(GenerateCpp.visit)
    val gathered = body.map(_.trim).groupBy(x => x.substring(0, x.indexOf('{') + 1)).map(x => {
      x._1 + n + x._2.map(y => y.substring(x._1.length + 1, y.length - 1)).mkString("") + n + "}"
    })
    a.map(GenerateCpp.forwardDecl(_)) ++
      Seq("#include \"generate.decl.h\"") ++
      gathered ++ Seq(n + "#include \"generate.def.h\"")
  }
}
