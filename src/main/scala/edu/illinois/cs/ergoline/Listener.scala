package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ErgolineParser._;

class Listener extends ErgolineBaseListener {
  private var node : ast.EirNode = null

  override def enterProgram(ctx: ErgolineParser.ProgramContext): Unit = {
    ctx.packageStatement().
  }
}
