package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.SemanticVal.{booleanEqualsInt, getLocalDefs}
import org.uwplse.liquid.spec.Literal.{BoolLit, IntLit}
import org.uwplse.liquid.spec.Utils.{OptBinding, optBinding}
import soot.{Local, Value}
import soot.jimple.{IntConstant, Stmt}

import scala.jdk.CollectionConverters._

case class SootValueContext(stmt: Stmt, methodEnv: MethodEnv)

sealed abstract class Expr extends Product with Serializable {
  def matches(value: Value, valueContext: SootValueContext) : OptBinding
}

object Expr {
  final case class LitExpr(l: Literal) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = {
      (l, value) match {
        case (IntLit(i), c:IntConstant) =>
          optBinding(i == c.value)
        case (BoolLit(b), c:IntConstant) =>
          optBinding(booleanEqualsInt(b, c.value))
        case (BoolLit(b), l:Local) =>
          val localDefs = getLocalDefs(valueContext.methodEnv.sootMethod)
          val defs = localDefs.getDefsOfAt(l, valueContext.stmt).asScala
          val constants = defs.map(i => {
            val stmt = i.asInstanceOf[Stmt]
            if (stmt.containsInvokeExpr() &&
              stmt.getInvokeExpr.getMethod.getSignature == "<java.lang.Boolean: java.lang.Boolean valueOf(boolean)>") {
              val intConstant = stmt.getInvokeExpr.getArg(0).asInstanceOf[IntConstant]
              Some(booleanEqualsInt(b, intConstant.value))
            } else {
              None
            }
          })
          if (constants.forall(_.isDefined)) {
            optBinding(constants.forall(_.get))
          } else {
            optBinding(false)
          }
        case _ => optBinding(false)
      }
    }
  }

  final case class VarExpr(binder: String) extends Expr {
    def matches(value: Value, valueContext: SootValueContext) : OptBinding = {
      Some(Map((binder, SemanticVal.SootValue(value, valueContext))))
    }
  }
}