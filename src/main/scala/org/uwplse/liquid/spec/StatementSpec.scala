package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.False
import soot.jimple.{InstanceInvokeExpr, InvokeExpr, Stmt}

import scala.jdk.CollectionConverters._

sealed abstract class Arguments extends Product with Serializable

object Arguments {
  final case class Contain(args: Set[Expr]) extends Arguments
  final case class Are(args: List[Expr]) extends Arguments
}

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : Constraint
}

object StatementSpec {
  final case class Invoke(name: String, args: Arguments) extends StatementSpec {
    def matches(appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : Constraint = {
      val methodSig = appSpec.findMethodSignature(name).get
      if (stmt.containsInvokeExpr()) {
        val argsToMatch = stmt.getInvokeExpr match {
          case i:InstanceInvokeExpr => List(i.getBase) ++ i.getArgs.asScala
          case i => i.getArgs.asScala
        }
        val argsConstraints = args match {
          case Arguments.Contain(_) => Constraint.True()
          case Arguments.Are(args) =>
            if (args.size == argsToMatch.size) {
              Constraint.foldAnd(args.zipWithIndex.map { case (arg, i) => arg.matches(argsToMatch(i))})
            } else {
              Constraint.False()
            }
        }
        methodSig.matches(stmt.getInvokeExpr.getMethod) && argsConstraints
      } else {
        False()
      }
    }
  }
}
