package org.uwplse.liquid.spec

import org.uwplse.liquid.{Analysis, Config}
import org.uwplse.liquid.spec.Utils._
import soot.Value
import soot.jimple.{InstanceInvokeExpr, Stmt}

import scala.jdk.CollectionConverters._

sealed abstract class Arguments extends Product with Serializable

object Arguments {
  final case class Contain(args: Set[Literal]) extends Arguments
  final case class Are(args: List[Expr]) extends Arguments
}

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : OptBinding
}

object StatementSpec {
  final case class Invoke(name: String, args: Arguments) extends StatementSpec {
    def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec, methodEnv: MethodEnv, stmt: Stmt) : OptBinding = {
      val methodSig = appSpec.findMethodSignature(name).get
      if (stmt.containsInvokeExpr()) {
        methodSig.matches(stmt.getInvokeExpr.getMethod) match {
          case Some(binding) =>
            val ctx = SootValueContext(stmt, methodEnv)
            val argsOptBinding: OptBinding = args match {
              case Arguments.Contain(litArgs) => {
                val constantFlowIns: Set[Value] = Analysis.getConstantFlowIns(stmt, config)
                optBinding(litArgs.forall(l => {
                  val ret = constantFlowIns.exists(v => l.matches(v, ctx))
                  ret
                }))
              }
              case Arguments.Are(args) =>
                val argsToMatch = stmt.getInvokeExpr match {
                  case i: InstanceInvokeExpr => List(i.getBase) ++ i.getArgs.asScala
                  case i => i.getArgs.asScala
                }
                if (args.size == argsToMatch.size) {
                  // TODO: finds dependencies of a specific arg as well to match with Regex case
                  args.zip(argsToMatch).map({ case (arg, argMatch) =>
                    arg.matches(argMatch, ctx)
                  }).fold(optBinding(true))(mergeOptBinding)
                } else {
                  None
                }
            }
            mergeOptBinding(Some(binding), argsOptBinding)
          case None => None
        }
      } else {
        optBinding(false)
      }
    }
  }
}
