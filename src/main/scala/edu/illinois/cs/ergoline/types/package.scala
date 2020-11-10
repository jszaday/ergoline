package edu.illinois.cs.ergoline

import edu.illinois.cs.ergoline.ast.EirScope
import edu.illinois.cs.ergoline.types.Allowed
import edu.illinois.cs.ergoline.util.EirResolvable
import edu.illinois.cs.ergoline.util.EirUtilitySyntax._

package object types {

  type Allowed = Either[EirType, EirResolvable[EirType]]

  trait EirType {
    def name: String

    override def toString: String = name
  }

  case class EirTupleType(var elements: List[Allowed]) extends EirType {
    def name: String = s"(${elements mkString ","})"
  }

  case class EirLambdaType(var from: Allowed, var to: Allowed) extends EirType {
    def name: String = s"$from => $to"
  }

  case class EirTemplatedType(var base: Allowed, var args: List[Allowed]) extends EirType {
    override def name: String = s"$base<${args mkString ","}>"
  }

  case class EirProxyType(var base: Allowed, var collective: Option[String]) extends EirType {
    override def name: String = s"$base@${collective.getOrElse("")}"
  }

  case class EirResolvableType(var scope: EirScope, var resolvable: EirResolvable[EirType]) extends EirType {
    override def name: String = resolvable.resolve(scope).get.name

    override def toString: String = s"Pending!$resolvable"
  }

  object EirUnitType extends EirType {
    override def name: String = "unit"
  }

  object EirResolvableType {
    def fromName(names: Iterable[String])(implicit scope: EirScope): EirResolvableType = {
      names.asResolvable[EirType].asType
    }
  }
}
