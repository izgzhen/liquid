package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.ExprEquals
import soot.Value
import soot.jimple.Stmt

case class SootValueContext(stmt: Stmt, methodEnv: MethodEnv)

sealed abstract class Expr extends Product with Serializable {
  def matches(value: Value, valueContext: SootValueContext) : Constraint = {
    ExprEquals(this, value, valueContext)
  }
}

object Expr {
  final case class LitExpr(l: Literal) extends Expr
  // NOTE: currently only attributes are matched against here
  final case class VarExpr(binder: String) extends Expr
}