package org.uwplse.liquid.analysis

import java.util

import heros.flowfunc.{Identity, KillAll}
import heros.{DefaultSeeds, FlowFunction, FlowFunctions, InterproceduralCFG}
import soot.jimple.internal.JimpleLocal
import soot.jimple.toolkits.ide.DefaultJimpleIFDSTabulationProblem
import soot.jimple.{DefinitionStmt, InstanceInvokeExpr, Jimple, ReturnStmt, ReturnVoidStmt, Stmt, StringConstant}
import soot.{EquivalentValue, Local, NullType, Scene, SootMethod, Value}
import SootUtils._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class DependencyProp(val icfg: InterproceduralCFG[soot.Unit, SootMethod])
  extends DefaultJimpleIFDSTabulationProblem[(Value, Set[ConstantVal]), InterproceduralCFG[soot.Unit, SootMethod]](icfg) {
  type Domain = (Value, Set[ConstantVal])
  private val id = Identity.v[Domain]()
  private val killAll = KillAll.v[Domain]()
  private val zero = zeroValue()
  type AbstractionMap = mutable.Map[soot.Unit, mutable.TreeMap[Value, Set[ConstantVal]]]
  private def initAbstractionMap() = new mutable.TreeMap[soot.Unit, mutable.TreeMap[Value, Set[ConstantVal]]]()(Ordering.by(_.toString()))
  val unitAbstractionMap : AbstractionMap = initAbstractionMap()
  val visitedMethods = new mutable.TreeSet[SootMethod]()(Ordering.by(_.toString()))

  private def putUnitAbstractions(u: soot.Unit, abstraction: Domain): Boolean = {
    unitAbstractionMap.getOrElseUpdate(u, mutable.TreeMap[Value, Set[ConstantVal]]()(Ordering.by(_.toString()))).addOne(abstraction)
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  override def createFlowFunctionsFactory(): FlowFunctions[soot.Unit, Domain, SootMethod] =
    new FlowFunctions[soot.Unit, Domain, SootMethod]() {
      override def getNormalFlowFunction(curr: soot.Unit, succ: soot.Unit): FlowFunction[Domain] = {
        curr match {
          case defStmt: DefinitionStmt =>
            source: Domain => {
              val (locals, constants) = getUsedVals(defStmt)
              val s: Set[Domain] = if (source != zero) {
                putUnitAbstractions(curr, source)
                val (tainted, taints) = source
                if (tainted.equivTo(defStmt.getLeftOp)) {
                  Set()
                } else if (locals.exists(tainted.equivTo)) {
                  Set((defStmt.getLeftOp, taints ++ constants))
                } else {
                  Set(source)
                }
              } else {
                if (constants.nonEmpty) {
                  Set((defStmt.getLeftOp, constants))
                } else {
                  Set()
                }
              }
              s.asJava
            }
          case _ => id
        }
      }

      override def getCallFlowFunction(callStmt: soot.Unit, destinationMethod: SootMethod): FlowFunction[Domain] = {
        val stmt = callStmt.asInstanceOf[Stmt]
        val invokeExpr = stmt.getInvokeExpr
        val args = invokeExpr.getArgs
        source: Domain => {
          val s: Set[Domain] = if (destinationMethod.getName == "<clinit>" || destinationMethod.getSubSignature == "void run()") {
            Set(source)
          } else if (source != zero) {
            putUnitAbstractions(callStmt, source)
            val (tainted, taints) = source
            if (args.contains(tainted)) {
              val paramIndex = args.indexOf(tainted)
              val param: Value = new EquivalentValue(Jimple.v.newParameterRef(destinationMethod.getParameterType(paramIndex), paramIndex))
              Set((param, taints))
            } else {
              Set()
            }
          } else {
            args.asScala.flatMap {
              case _:Local => None
              case arg =>
                val constants = getUsedConstantVals(arg)
                if (constants.nonEmpty) {
                  val paramIndex = args.indexOf(arg)
                  val param: Value = new EquivalentValue(Jimple.v.newParameterRef(destinationMethod.getParameterType(paramIndex), paramIndex))
                  Some((param, constants))
                } else {
                  None
                }
            }.toSet
          }
          s.asJava
        }
      }

      override def getReturnFlowFunction(callSite: soot.Unit, calleeMethod: SootMethod,
                                         exitStmt: soot.Unit, returnSite: soot.Unit): FlowFunction[Domain] = {
        callSite match {
          case defStmt:DefinitionStmt =>
            exitStmt match {
              case _:ReturnVoidStmt => killAll
              case retStmt:ReturnStmt =>
                source: Domain => {
                  val s: Set[Domain] = if (source != zero) {
                    putUnitAbstractions(exitStmt, source)
                    val (tainted, taints) = source
                    if (retStmt.getOp.equivTo(tainted)) {
                      Set((defStmt.getLeftOp, taints))
                    } else {
                      Set()
                    }
                  } else {
                    Set()
                  }
                  s.asJava
                }
            }
          case _ => killAll
        }
      }

      override def getCallToReturnFlowFunction(callSite: soot.Unit, returnSite: soot.Unit): FlowFunction[Domain] = {
        callSite match {
          case defStmt:DefinitionStmt =>
            val invokeExpr = defStmt.getInvokeExpr
            if (invokeExpr.getMethod.getDeclaringClass.isApplicationClass) {
              id
            } else {
              source: Domain => {
                val s: Set[Domain] = if (source != zero) {
                  putUnitAbstractions(callSite, source)
                  invokeExpr match {
                    case instanceInvokeExpr: InstanceInvokeExpr =>
                      instanceInvokeExpr.getMethod.getSignature match {
                        case "<java.lang.StringBuilder: java.lang.StringBuilder append(java.lang.String)>" =>
                          Set(source)
                        case _ => Set(source)
                      }
                    case _ => Set(source)
                  }
                } else {
                  Set(source)
                }
                s.asJava
              }
            }
          case stmt: Stmt =>
            val invokeExpr = stmt.getInvokeExpr
            source: Domain => {
              val s: Set[Domain] = if (source != zero) {
                val (tainted, taints) = source
                invokeExpr match {
                  case instanceInvokeExpr: InstanceInvokeExpr =>
                    instanceInvokeExpr.getMethod.getSignature match {
                      case "<java.lang.StringBuilder: java.lang.StringBuilder append(java.lang.String)>" =>
                        if (tainted.equivTo(instanceInvokeExpr.getBase)) {
                          invokeExpr.getArg(0) match {
                            case strConst:StringConstant =>
                              Set((tainted, taints.map {
                                case ConstantVal.IntConst(i) => ConstantVal.StrConst(i.toString + strConst.value)
                                case ConstantVal.StrConst(s) => ConstantVal.StrConst(s + strConst.value)
                              }))
                            case _ => Set(source)
                          }
                        } else if (tainted.equivTo(instanceInvokeExpr.getArg(0))) {
                          Set((instanceInvokeExpr.getBase, taints))
                        } else {
                          Set(source)
                        }
                      case _ => Set(source)
                    }
                  case _ => Set(source)
                }
              } else {
                invokeExpr match {
                  case instanceInvokeExpr: InstanceInvokeExpr => {
                    invokeExpr.getMethod.getSignature match {
                      case "<java.lang.StringBuilder: void <init>()>" =>
                        Set((instanceInvokeExpr.getBase, Set(ConstantVal.StrConst(""))))
                      case "<java.lang.StringBuilder: java.lang.StringBuilder append(java.lang.String)>" =>
                        val constants = getUsedConstantVals(invokeExpr.getArg(0))
                        if (constants.nonEmpty) {
                          Set((instanceInvokeExpr.getBase, constants))
                        } else {
                          Set(source)
                        }
                      case _ => Set(source)
                    }
                  }
                  case _ => Set(source)
                }
              }
              s.asJava
            }
        }
      }
    }

  // FIXME: entrypoints might not be enough
  override def initialSeeds(): util.Map[soot.Unit, util.Set[(Value, Set[ConstantVal])]] = {
    DefaultSeeds.make(Scene.v.getEntryPoints.asScala.filter(_.hasActiveBody).map(_.getActiveBody.getUnits.getFirst).asJava, zeroValue())
  }

  override def createZeroValue: (Value, Set[ConstantVal]) = (new JimpleLocal("<<zero>>", NullType.v), Set())
}
