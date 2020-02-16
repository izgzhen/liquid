package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.Binding
import org.uwplse.liquid.spec.Expr.VarExpr
import org.uwplse.liquid.{Analysis, Config}
import org.uwplse.liquid.spec.Utils._
import soot.{Local, Value}
import soot.jimple.{DefinitionStmt, InstanceInvokeExpr, Stmt}

import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed abstract class Arguments extends Product with Serializable

object Arguments {
  final case class Contain(args: Set[Literal]) extends Arguments
  final case class Are(args: List[Expr]) extends Arguments
}

sealed abstract class StatementSpec extends Product with Serializable {
  def matches(appSpec: AppSpec, classSpec: ClassSpec,
              methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : Option[Binding]
//  def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
//               methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : ScoredBinding
}

object StatementSpec {
  val invokeMatches : mutable.Map[(Invoke, AppSpec, ClassSpec,  MethodEnv, Stmt, Binding), Option[Binding]] = mutable.Map()

  final case class Invoke(name: String, args: Arguments, lhsBinder: Option[String]) extends StatementSpec {
    def matches(appSpec: AppSpec, classSpec: ClassSpec,
                methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : Option[Binding] = {
      val key = (this, appSpec, classSpec, methodEnv, stmt, ctx)
      if (!invokeMatches.contains(key)) {
        val value = _matches(appSpec, classSpec, methodEnv, stmt, ctx)
        invokeMatches.addOne(key, value)
      }
      invokeMatches(key)
    }

    def _matches(appSpec: AppSpec, classSpec: ClassSpec,
                 methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : Option[Binding] = {
      if (stmt.containsInvokeExpr()) {
        val optInvokedBinding = ctx.m.get(name) match {
          case Some(value) =>
            val matchedMethod = value.asInstanceOf[SemanticVal.Method].m
            if (matchedMethod == stmt.getInvokeExpr.getMethod) {
              Some(Binding.one())
            } else {
              None
            }
          case None =>
            Some(Binding(Map(name -> SemanticVal.Method(stmt.getInvokeExpr.getMethod))))
        }
        optInvokedBinding match {
          case Some(invokedBinding) =>
            val valCtx = SootValueContext(stmt, methodEnv)
            val argsOptBinding: OptBinding = args match {
              case Arguments.Contain(litArgs) => {
                if (litArgs.isEmpty) {
                  optBinding(true)
                }
                else {
                  val constantFlowIns: Set[Value] = Analysis.getConstantFlowIns(stmt)
                  optBinding(litArgs.forall(l => {
                    val ret = constantFlowIns.exists(v => l.matches(v, valCtx))
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
                    arg.matches(argMatch, valCtx)
                  }).fold(optBinding(true))(mergeOptBinding)
                } else {
                  None
                }
            }
            val retBinding: Option[Binding] = (stmt, lhsBinder) match {
              case (defStmt: DefinitionStmt, Some(binder)) =>
                defStmt.getLeftOp match {
                  case l: Local => VarExpr(binder).matches(l, valCtx)
                  case _ => None
                }
              case (_, None) => Some(Binding.one())
              case _ => None
            }
            mergeOptBinding(mergeOptBinding(retBinding, argsOptBinding), Some(invokedBinding))
          case None => None
        }
      } else {
        optBinding(false)
      }
    }

//    def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec,
//                 methodEnv: MethodEnv, stmt: Stmt, ctx: Binding) : ScoredBinding = {
//      if (stmt.containsInvokeExpr()) {
//        val matchedMethod = ctx(name).asInstanceOf[SemanticVal.Method].m
//        if (matchedMethod == stmt.getInvokeExpr.getMethod) {
//          val ctx = SootValueContext(stmt, methodEnv)
//          val argsOptBinding: ScoredBinding = args match {
//            case Arguments.Contain(litArgs) => {
//              if (litArgs.isEmpty) { scoredBindingTrue() }
//              else {
//                val constantFlowIns: Set[Value] = Analysis.getConstantFlowIns(stmt, config)
//                val score = litArgs.map(l => {
//                  val ret = constantFlowIns.map(v => l.matchesR(v, ctx)).max
//                  ret
//                }).fold(1.0)(scoreProd)
//                scoredBindingFrom(score)
//              }
//            }
//            case Arguments.Are(args) =>
//              val argsToMatch = stmt.getInvokeExpr match {
//                case i: InstanceInvokeExpr => List(i.getBase) ++ i.getArgs.asScala
//                case i => i.getArgs.asScala
//              }
//              if (args.size == argsToMatch.size) {
//                // TODO: finds dependencies of a specific arg as well to match with Regex case
//                args.zip(argsToMatch).map({ case (arg, argMatch) =>
//                  arg.matchesR(argMatch, ctx)
//                }).fold(scoredBindingTrue())(mergeScoredBinding)
//              } else {
//                scoredBindingFalse()
//              }
//          }
//          val retBinding = (stmt, lhsBinder) match {
//            case (defStmt:DefinitionStmt, Some(binder)) =>
//              defStmt.getLeftOp match {
//                case l:Local => VarExpr(binder).matchesR(l, ctx)
//                case _ => scoredBindingFalse()
//              }
//            case (_, None) => scoredBindingTrue()
//            case _ => scoredBindingFalse()
//          }
//          mergeScoredBinding(retBinding, argsOptBinding)
//        } else {
//          scoredBindingFalse()
//        }
//      } else {
//        scoredBindingFalse()
//      }
//    }
  }
}
