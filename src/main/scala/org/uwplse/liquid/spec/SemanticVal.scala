package org.uwplse.liquid.spec

import soot.Value
sealed abstract class SemanticVal extends Product with Serializable

object SemanticVal {
  final case class SootValue(v: Value, ctx: SootValueContext) extends SemanticVal
  final case class Name(name: String) extends SemanticVal
}