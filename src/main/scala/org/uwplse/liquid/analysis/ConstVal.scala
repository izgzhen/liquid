package org.uwplse.liquid.analysis

sealed abstract class ConstVal extends Product with Serializable

object ConstVal {
  final case class StringConstant(s: String) extends ConstVal
  final case class IntegerConstant(i: Int) extends ConstVal
}