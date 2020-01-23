package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils._
import soot.jimple.{InstanceInvokeExpr, Stmt}

import scala.jdk.CollectionConverters._

sealed abstract class Arguments extends Product with Serializable

object Arguments {
  final case class Contain(args: Set[Expr]) extends Arguments
  final case class Are(args: List[Expr]) extends Arguments
}

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : OptBinding
}

object StatementSpec {
  final case class Invoke(name: String, args: Arguments) extends StatementSpec {
    def matches(appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : OptBinding = {
      val methodSig = appSpec.findMethodSignature(name).get
      if (stmt.containsInvokeExpr()) {
        val argsToMatch = stmt.getInvokeExpr match {
          case i:InstanceInvokeExpr => List(i.getBase) ++ i.getArgs.asScala
          case i => i.getArgs.asScala
        }
        val argsOptBinding: OptBinding = args match {
          case Arguments.Contain(_) => optBinding(true)
          case Arguments.Are(args) =>
            if (args.size == argsToMatch.size) {
              args.zip(argsToMatch).map({ case (arg, argMatch) =>
                arg.matches(argMatch, SootValueContext(stmt, methodEnv))}).fold(optBinding(true))(mergeOptBinding)
            } else {
              None
            }
        }
        val sigMatch = methodSig.matches(stmt.getInvokeExpr.getMethod)
        mergeOptBinding(sigMatch, argsOptBinding)
      } else {
        optBinding(false)
      }
    }
  }
}
