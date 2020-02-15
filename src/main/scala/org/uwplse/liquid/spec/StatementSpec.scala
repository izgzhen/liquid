package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Expr.VarExpr
import org.uwplse.liquid.spec.IdentifierPattern.NamedWildcard
import org.uwplse.liquid.{Analysis, Config}
import org.uwplse.liquid.spec.Utils._
import soot.{Local, Value}
import soot.jimple.{DefinitionStmt, InstanceInvokeExpr, Stmt}

import scala.jdk.CollectionConverters._

sealed abstract class Arguments extends Product with Serializable

object Arguments {
  final case class Contain(args: Set[Literal]) extends Arguments
  final case class Are(args: List[Expr]) extends Arguments
}

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
              methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : OptBinding
  def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
               methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : ScoredBinding
}

object StatementSpec {
  final case class Invoke(name: IdentifierPattern, args: Arguments, lhsBinder: Option[String]) extends StatementSpec {
    def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
                methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : OptBinding = {
      if (stmt.containsInvokeExpr()) {
        val methodSymbolicName = name.asInstanceOf[NamedWildcard].binder // FIXME
        val matchedMethod = ctx(methodSymbolicName).asInstanceOf[SemanticVal.Method].m
        if (matchedMethod == stmt.getInvokeExpr.getMethod) {
            val ctx = SootValueContext(stmt, methodEnv)
            val argsOptBinding: OptBinding = args match {
              case Arguments.Contain(litArgs) => {
                if (litArgs.isEmpty) { optBinding(true) }
                else {
                  val constantFlowIns: Set[Value] = Analysis.getConstantFlowIns(stmt, config)
                  optBinding(litArgs.forall(l => {
                    val ret = constantFlowIns.exists(v => l.matches(v, ctx))
                    ret
                  }))
                }
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
            val retBinding = (stmt, lhsBinder) match {
              case (defStmt:DefinitionStmt, Some(binder)) =>
                defStmt.getLeftOp match {
                  case l:Local => VarExpr(binder).matches(l, ctx)
                  case _ => None
                }
              case (_, None) => optBinding(true)
              case _ => optBinding(false)
            }
            mergeOptBinding(retBinding, argsOptBinding)
        } else {
          None
        }
      } else {
        optBinding(false)
      }
    }

    def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
                 methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : ScoredBinding = {
      if (stmt.containsInvokeExpr()) {
        val methodSymbolicName = name.asInstanceOf[NamedWildcard].binder // FIXME
        val matchedMethod = ctx(methodSymbolicName).asInstanceOf[SemanticVal.Method].m
        if (matchedMethod == stmt.getInvokeExpr.getMethod) {
          val ctx = SootValueContext(stmt, methodEnv)
          val argsOptBinding: ScoredBinding = args match {
            case Arguments.Contain(litArgs) => {
              if (litArgs.isEmpty) { scoredBindingTrue() }
              else {
                val constantFlowIns: Set[Value] = Analysis.getConstantFlowIns(stmt, config)
                val score = litArgs.map(l => {
                  val ret = constantFlowIns.map(v => l.matchesR(v, ctx)).max
                  ret
                }).fold(1.0)(scoreProd)
                scoredBindingFrom(score)
              }
            }
            case Arguments.Are(args) =>
              val argsToMatch = stmt.getInvokeExpr match {
                case i: InstanceInvokeExpr => List(i.getBase) ++ i.getArgs.asScala
                case i => i.getArgs.asScala
              }
              if (args.size == argsToMatch.size) {
                // TODO: finds dependencies of a specific arg as well to match with Regex case
                args.zip(argsToMatch).map({ case (arg, argMatch) =>
                  arg.matchesR(argMatch, ctx)
                }).fold(scoredBindingTrue())(mergeScoredBinding)
              } else {
                scoredBindingFalse()
              }
          }
          val retBinding = (stmt, lhsBinder) match {
            case (defStmt:DefinitionStmt, Some(binder)) =>
              defStmt.getLeftOp match {
                case l:Local => VarExpr(binder).matchesR(l, ctx)
                case _ => scoredBindingFalse()
              }
            case (_, None) => scoredBindingTrue()
            case _ => scoredBindingFalse()
          }
          mergeScoredBinding(retBinding, argsOptBinding)
        } else {
          scoredBindingFalse()
        }
      } else {
        scoredBindingFalse()
      }
    }
  }
}
