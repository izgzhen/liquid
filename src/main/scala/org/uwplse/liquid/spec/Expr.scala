package org.uwplse.liquid.spec

sealed abstract class Expr extends Product with Serializable

object Expr {
  final case class LitExpr(l: Literal) extends Expr
  // NOTE: currently only attributes are matched against here
  final case class VarExpr(name: String) extends Expr
}