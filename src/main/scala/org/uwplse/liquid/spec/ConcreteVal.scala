package org.uwplse.liquid.spec

import soot.{SootMethod, Value}
sealed abstract class ConcreteVal extends Product with Serializable

object ConcreteVal {
  final case class SootValue(v: Value, ctx: SootValueContext) extends ConcreteVal
  final case class Name(name: String) extends ConcreteVal
  final case class Method(m: SootMethod) extends ConcreteVal
}