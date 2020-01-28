package org.uwplse.liquid.spec

sealed abstract class Literal extends Product with Serializable

object Literal {
  final case class StringLit(str: String) extends Literal
  final case class IntLit(int: Int) extends Literal
  final case class BoolLit(b: Boolean) extends Literal
  final case class RegexLit(r: String) extends Literal
}
