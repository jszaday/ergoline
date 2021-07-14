package edu.illinois.cs.ergoline.proxies

import edu.illinois.cs.ergoline.ast.{EirFunction, EirMember}
import edu.illinois.cs.ergoline.passes.TypeCheckContext
import edu.illinois.cs.ergoline.util.Errors

object CheckProxy {

  // validate base has an entry constructor of the appropriate nature
  def apply(x: EirProxy)(implicit ctx: TypeCheckContext): EirProxy = {
    val baseMembers = x.baseMembers
    val constructors = x.baseConstructors()

    if (constructors.exists(_.annotation("threaded").isDefined)) {
      Errors.unsupportedOperation(
        x.base,
        "threaded @entry constructors are unsupported",
        Errors.Limitation.CharmxiCodeGen
      )
    }

    val numConstructors = constructors.size
    val defaultConstructors = constructors flatMap (_.counterpart) filter {
      case EirMember(_, f: EirFunction, _) => f.functionArgs.isEmpty
      case _                               => false
    }

    if (
      numConstructors > 1 && !(numConstructors == 2 && defaultConstructors.size == 1)
    ) {
      Errors.unsupportedOperation(
        x.base,
        "multiple constructors are unsupported",
        Errors.Limitation.CharmxiCodeGen
      )
    }

    if (
      constructors exists {
        case EirMember(_, f: EirFunction, _) => f.templateArgs.nonEmpty
        case _                               => false
      }
    ) {
      Errors.unsupportedOperation(
        x.base,
        "templated constructors are unsupported",
        Errors.Limitation.CharmxiCodeGen
      )
    }

    val creators =
      baseMembers.filter(_.annotations.exists(_.name.startsWith("create")))
    if (x.isArray && creators.nonEmpty && defaultConstructors.isEmpty) {
      Errors.expectedDefaultConstructible(x.base)
    }

    x
  }
}
