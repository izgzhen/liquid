package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.ExprEquals
import soot.Value

sealed abstract class Expr extends Product with Serializable {
  def matches(value: Value) : Constraint = {
    ExprEquals(this, value)
  }
}

object Expr {
  final case class LitExpr(l: Literal) extends Expr
  // NOTE: currently only attributes are matched against here
  final case class VarExpr(binder: String) extends Expr
}