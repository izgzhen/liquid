package org.uwplse.liquid.analysis

import java.util

import heros.flowfunc.{Identity, KillAll}
import heros.{FlowFunction, FlowFunctions, InterproceduralCFG}
import soot.jimple.internal.JimpleLocal
import soot.jimple.{IntConstant, Stmt, StringConstant}
import soot.{Local, NullType, SootMethod, Value}
import soot.jimple.toolkits.ide.DefaultJimpleIFDSTabulationProblem

import scala.collection.mutable
import scala.jdk.CollectionConverters._

class DependencyForwardProp(val icfg: InterproceduralCFG[soot.Unit, SootMethod], val initMap: mutable.Map[soot.Unit, mutable.TreeSet[Value]], val sink: Stmt)
  extends DefaultJimpleIFDSTabulationProblem[Value, InterproceduralCFG[soot.Unit, SootMethod]](icfg) {
  private val id = Identity.v[Value]()
  private val killAll = KillAll.v[Value]()
  private val zero = createZeroValue()
  type abstractionMap = mutable.Map[soot.Unit, mutable.TreeSet[Value]]
  private def initAbstractionMap() = new mutable.TreeMap[soot.Unit, mutable.TreeSet[Value]]()(Ordering.by(_.toString()))
  val unitAbstractionMap     : abstractionMap = initAbstractionMap()
  val unitAbstractionAfterMap: abstractionMap = initAbstractionMap()
  val visitedMethods = new mutable.TreeSet[SootMethod]()(Ordering.by(_.toString()))

  private def putUnitAbstractions(u: soot.Unit, abstraction: Value): Boolean = {
    unitAbstractionMap.getOrElseUpdate(u, mutable.TreeSet[Value]()(Ordering.by(_.toString()))).add(abstraction)
    visitedMethods.add(interproceduralCFG.getMethodOf(u))
  }

  private def putUnitAbstractionsAfter(u: soot.Unit, flow: Iterable[Value]): Boolean = {
    for (abstraction <- flow) {
      unitAbstractionAfterMap
        .getOrElseUpdate(u, mutable.TreeSet[Value]()(Ordering.by(_.toString()))).add(abstraction)
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
            val flow: Set[Value] = if (curr.getDefBoxes.size() > 0 && curr.getDefBoxes.get(0).getValue.equivTo(source)) {
              Set()
            } else {
              val sourceIsUsed = curr.getUseBoxes.asScala.exists(_.getValue == source)
              if (sourceIsUsed && source.isInstanceOf[Local]) {
                getTaintedValues(curr.asInstanceOf[Stmt])
              } else {
                Set(source)
              }
            }
            putUnitAbstractionsAfter(curr, flow)
            flow.asJava
          } else {
            Set(source).asJava
          }
        }
      }

      override def getCallFlowFunction(callStmt: soot.Unit, destinationMethod: SootMethod): FlowFunction[Value] = {
        killAll
      }

      override def getReturnFlowFunction(callSite: soot.Unit, calleeMethod: SootMethod,
                                         exitStmt: soot.Unit, returnSite: soot.Unit): FlowFunction[Value] = {
        killAll
      }

      override def getCallToReturnFlowFunction(callSite: soot.Unit, returnSite: soot.Unit): FlowFunction[Value] = {
        if (callSite == sink) {
          return killAll
        }
        if (callSite.getUseAndDefBoxes.isEmpty) {
          return id
        }
        source: Value => {
          putUnitAbstractions(callSite, source)
          val defs = callSite.getDefBoxes
          val flow: Set[Value] = if (!defs.isEmpty && defs.get(0).getValue.equivTo(source)) {
            Set()
          } else {
            val sourceIsUsed = callSite.getUseBoxes.asScala.exists(_.getValue.equivTo(source))
            if (sourceIsUsed && source.isInstanceOf[Local]) {
              getTaintedValues(callSite.asInstanceOf[Stmt])
            } else {
              Set(source)
            }
          }
          putUnitAbstractionsAfter(callSite, flow)
          flow.asJava
        }
      }
    }

  def getTaintedValues(s: Stmt): Set[Value] = {
    (s.getUseBoxes.asScala ++ s.getDefBoxes.asScala).map(_.getValue).filter {
      case _:Local => true
      case _:IntConstant => true
      case _:StringConstant => true
      case _ => false
    }.toSet
  }

  override def initialSeeds(): util.Map[soot.Unit, util.Set[Value]] = {
    initMap.map { case (k, v) => (k, v.toSet.asJava )} .asJava
  }

  override def createZeroValue = new JimpleLocal("<<zero>>", NullType.v)
}
