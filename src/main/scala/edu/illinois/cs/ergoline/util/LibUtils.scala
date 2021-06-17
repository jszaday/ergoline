package edu.illinois.cs.ergoline.util

object LibUtils {
  private var result: Option[String] = None

  private def checkResult(s: String): Boolean =
    result.exists(_.contains("lib" + s))

  def isPresent(lib: String): Boolean = {
    checkResult(lib) || (result.isEmpty && {
      try {
        result = Some(os.proc(List("ldconfig", "-p")).call().out.text())
        checkResult(lib)
      } catch {
        case _: Throwable => false
      }
    })
  }

  def linkLib(lib: String): Option[String] = {
    Option.when(isPresent(lib))("-l" + lib)
  }
}
