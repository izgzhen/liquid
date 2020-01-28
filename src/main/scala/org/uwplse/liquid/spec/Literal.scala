package org.uwplse.liquid.spec

import soot.{Local, Value}
import soot.jimple.{IntConstant, Stmt, StringConstant}
import org.uwplse.liquid.Analysis.{booleanEqualsInt, getLocalDefs}

import scala.jdk.CollectionConverters._

sealed abstract class Literal extends Product with Serializable {
  def matches(value: Value, valueContext: SootValueContext): Boolean
}
object Literal {
  final case class StringLit(str: String) extends Literal {
    def matches(value: Value, valueContext: SootValueContext): Boolean = {
      value match {
        case c:StringConstant => c.value == str
        case _ => false
      }
    }
  }

  final case class IntLit(i: Int) extends Literal {
    def matches(value: Value, valueContext: SootValueContext): Boolean = {
      value match {
        case c:IntConstant => i == c.value
        case _ => false
      }
    }
  }

  final case class BoolLit(b: Boolean) extends Literal {
    def matches(value: Value, valueContext: SootValueContext): Boolean = {
      value match {
        case c:IntConstant => booleanEqualsInt(b, c.value)
        case l:Local =>
          val localDefs = getLocalDefs(valueContext.methodEnv.sootMethod)
          val defs = localDefs.getDefsOfAt(l, valueContext.stmt).asScala
          val constants = defs.map(i => {
            val stmt = i.asInstanceOf[Stmt]
            if (stmt.containsInvokeExpr() &&
              stmt.getInvokeExpr.getMethod.getSignature == "<java.lang.Boolean: java.lang.Boolean valueOf(boolean)>") {
              val intConstant = stmt.getInvokeExpr.getArg(0).asInstanceOf[IntConstant]
              booleanEqualsInt(b, intConstant.value)
            } else {
              false
            }
          })
          constants.forall(identity)
        case _ => false
      }
    }
  }

  final case class RegexLit(r: String) extends Literal {
    def matches(value: Value, valueContext: SootValueContext): Boolean = {
      value match {
        case s:StringConstant => r.r.matches(s.value)
        case _ => false
      }
    }
  }
}