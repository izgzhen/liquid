package org.uwplse.liquid.analysis

import java.util

import heros.flowfunc.Identity
import heros.{FlowFunction, FlowFunctions, InterproceduralCFG}
import soot.jimple.internal.JimpleLocal
import soot.jimple.{IntConstant, ReturnStmt, Stmt, StringConstant}
import soot.{Local, NullType, SootMethod, Value}
import soot.jimple.toolkits.ide.DefaultJimpleIFDSTabulationProblem

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class DependencyBackProp(val icfg: InterproceduralCFG[soot.Unit, SootMethod], val sink: Stmt)
  extends DefaultJimpleIFDSTabulationProblem[Value, InterproceduralCFG[soot.Unit, SootMethod]](icfg) {
  private val id = Identity.v[Value]()
  private val zero = createZeroValue()
  type abstractionMap = mutable.HashMap[soot.Unit, mutable.HashSet[Value]]
  private def initAbstractionMap() = new mutable.HashMap[soot.Unit, mutable.HashSet[Value]]()
  val unitAbstractionMap     : abstractionMap = initAbstractionMap()
  val unitAbstractionAfterMap: abstractionMap = initAbstractionMap()
  val visitedMethods = new mutable.HashSet[SootMethod]()

  private def putUnitAbstractions(u: soot.Unit, abstraction: Value): Unit = {
    unitAbstractionMap.getOrElseUpdate(u, mutable.HashSet[Value]()).add(abstraction)
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  private def putUnitAbstractionsAfter(u: soot.Unit, flow: Iterable[Value]): Unit = {
    for (abstraction <- flow) {
      unitAbstractionAfterMap
        .getOrElseUpdate(u, mutable.HashSet[Value]()).add(abstraction)
    }
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  override def createFlowFunctionsFactory(): FlowFunctions[soot.Unit, Value, SootMethod] =
    new FlowFunctions[soot.Unit, Value, SootMethod]() {
      override def getNormalFlowFunction(curr: soot.Unit, succ: soot.Unit): FlowFunction[Value] = {
        if (curr.getUseAndDefBoxes.isEmpty) {
          return id
        }
        source: Value => {
          if (source != zero) {
            putUnitAbstractions(curr, source)
            val flow = if (curr.getDefBoxes.size() > 0 && curr.getDefBoxes.get(0).getValue.equivTo(source)) {
              getUsedValues(curr.asInstanceOf[Stmt])
            } else {
              Set(source)
            }
            putUnitAbstractionsAfter(curr, flow)
            flow.asJava
          } else {
            Set(source).asJava
          }
        }
      }

      override def getCallFlowFunction(callStmt: soot.Unit, destinationMethod: SootMethod): FlowFunction[Value] = {
        source: Value => {
          putUnitAbstractions(callStmt, source)
          val calleeSideReturnValues = mutable.Set[Value]()
          if (!callStmt.getDefBoxes.isEmpty) {
            val callerSideReturnValue = callStmt.getDefBoxes.get(0).getValue
            if (callerSideReturnValue.equivTo(source)) {
              for (calleeUnit <- interproceduralCFG.getStartPointsOf(destinationMethod).asScala) {
                calleeUnit match {
                  case returnStmt: ReturnStmt =>
                    calleeSideReturnValues.add(returnStmt.getOp)
                  case _ =>
                }
              }
            }
          } else {
            // no return value, nothing to propagate
          }
          putUnitAbstractionsAfter(callStmt, calleeSideReturnValues)
          calleeSideReturnValues.asJava
        }
      }

      override def getReturnFlowFunction(callSite: soot.Unit, calleeMethod: SootMethod,
                                         exitStmt: soot.Unit, returnSite: soot.Unit): FlowFunction[Value] = {
        val s = callSite.asInstanceOf[Stmt]
        val invokeExpr = s.getInvokeExpr
        val callArgs = invokeExpr.getArgs.asScala
        val paramLocals = calleeMethod.getActiveBody.getParameterLocals.asScala
        source: Value =>
          putUnitAbstractions(exitStmt, source)
          val flow = paramLocals.zipWithIndex.filter(_._1 equivTo source).map { case (_, i) => callArgs(i) }.toSet
          putUnitAbstractionsAfter(exitStmt, flow)
          flow.asJava
      }

      override def getCallToReturnFlowFunction(callSite: soot.Unit, returnSite: soot.Unit): FlowFunction[Value] = {
        if (callSite.getUseAndDefBoxes.isEmpty) {
          return id
        }
        source: Value => {
          putUnitAbstractions(callSite, source)
          val defs = callSite.getDefBoxes
          val flow = if (!defs.isEmpty && defs.get(0).getValue.equivTo(source)) {
            getUsedValues(callSite.asInstanceOf[Stmt])
          } else {
            Set[Value](source)
          }
          putUnitAbstractionsAfter(callSite, flow)
          flow.asJava
        }
      }
    }

  def getUsedValues(s: Stmt): Set[Value] = {
    s.getUseBoxes.asScala.map(_.getValue).filter {
      case _:Local => true
      case _:IntConstant => true
      case _:StringConstant => true
      case _ => false
    }.toSet
  }

  override def initialSeeds(): util.Map[soot.Unit, util.Set[Value]] = {
    java.util.Collections.singletonMap(sink, getUsedValues(sink).asJava)
  }

  override def createZeroValue = new JimpleLocal("<<zero>>", NullType.v)
}
