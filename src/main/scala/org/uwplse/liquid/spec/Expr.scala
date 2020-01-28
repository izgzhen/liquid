package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils.{OptBinding, optBinding}
import soot.Value
import soot.jimple.Stmt


case class SootValueContext(stmt: Stmt, methodEnv: MethodEnv)

sealed abstract class Expr extends Product with Serializable {
  def matches(value: Value, valueContext: SootValueContext) : OptBinding
}

object Expr {
  final case class LitExpr(l: Literal) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = optBinding(l.matches(value, valueContext))
  }

  final case class VarExpr(binder: String) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = {
      Some(Map((binder, SemanticVal.SootValue(value, valueContext))))
    }
  }
}