package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.analysis.Binding
import org.uwplse.liquid.spec.Utils._
import soot.Value
import soot.jimple.Stmt

case class SootValueContext(stmt: Stmt, methodEnv: MethodEnv)

sealed abstract class Expr extends Product with Serializable {
  def matches(value: Value, valueContext: SootValueContext) : OptBinding
//  def matchesR(value: Value, valueContext: SootValueContext) : ScoredBinding = scoreOptBinding(matches(value, valueContext))
}

object Expr {
  final case class LitExpr(l: Literal) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = optBinding(l.matches(value, valueContext))

//    override def matchesR(value: Value, valueContext: SootValueContext): (Map[String, SemanticVal], Double) = {
//      val y = l.matchesR(value, valueContext)
//      scoredBindingProd(scoreOptBinding(matches(value, valueContext)), y)
//    }
  }

  final case class VarExpr(binder: String) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = {
      if (Analysis.typeMatch(valueContext.methodEnv.methodSpec.locals(binder), value.getType)) {
        Some(Binding(Map((binder, SemanticVal.SootValue(value, valueContext)))))
      } else {
        None
      }
    }
  }
}