package org.uwplse.liquid.analysis

sealed abstract class Abstraction extends Product with Serializable

object Abstraction {
  final case class StringConstant(s: String) extends Abstraction
  final case class IntegerConstant(i: Int) extends Abstraction
}