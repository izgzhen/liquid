package org.uwplse.liquid.analysis

sealed abstract class ConstantVal extends Product with Serializable

object ConstantVal {
  final case class IntConst(i: Int) extends ConstantVal
  final case class StrConst(s: String) extends ConstantVal
}
