package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.False
import soot.jimple.Stmt

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(appSpec: AppSpec, classSpec: ClassSpec, stmt: Stmt) : Constraint
}

object StatementSpec {
  final case class Invoke(name: String, args: List[Expr]) extends StatementSpec {
    def matches(appSpec: AppSpec, classSpec: ClassSpec, stmt: Stmt) : Constraint = {
      val methodSig = appSpec.findMethodSignature(name).get
      if (stmt.containsInvokeExpr()) {
        val invokeExpr = stmt.getInvokeExpr
        //        val argsToMatch = invokeExpr.getArgs
        methodSig.matches(invokeExpr.getMethod)
        //        match {
        //          case Some(matchedMethodSig) => {
        //            println(argsToMatch)
        //            args.forall()
        //              arg match {
        //                case Expr.LitExpr(l) => println("WARNING: unchecked literal " + l)
        //                case Expr.IdExpr(id) => {
        //                  for (attr <- classSpec.attributes) {
        //                    (attr.`type`.matches())
        //                  }
        //                }
        //              }
        //            }
        //            Some(MatchedInvokeStmt(matchedMethodSig))
        // TODO: check arg type match
        //   and emit wildcard binding constraint
        //   binding should be checked over equality of aliasing resolution
        //   but it seems very ambiguous ...
        //   Maybe a more explicit constraint solving process?
        // TODO: I might need some stream API to emit constraints properly
      } else {
        False()
      }
    }
  }
}
