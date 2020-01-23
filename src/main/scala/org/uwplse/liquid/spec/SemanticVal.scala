package org.uwplse.liquid.spec

import soot.{Local, SootMethod, Value}
import soot.jimple.Stmt
import soot.jimple.toolkits.pointer.LocalMustAliasAnalysis
import soot.toolkits.graph.CompleteUnitGraph
import soot.toolkits.scalar.{SimpleLiveLocals, SmartLocalDefs}

sealed abstract class SemanticVal extends Product with Serializable

object SemanticVal {
  final case class SootValue(v: Value, ctx: SootValueContext) extends SemanticVal
  final case class Name(name: String) extends SemanticVal

  val aliasAnalysisMap: scala.collection.mutable.Map[SootMethod, LocalMustAliasAnalysis] = scala.collection.mutable.Map()
  val localDefsMap: scala.collection.mutable.Map[SootMethod, SmartLocalDefs] = scala.collection.mutable.Map()

  def getLocalDefs(m: SootMethod) : SmartLocalDefs = {
    if (!localDefsMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      localDefsMap.addOne(m, new SmartLocalDefs(ug, new SimpleLiveLocals(ug)))
    }
    localDefsMap(m)
  }

  def isAlias(l1: Local, l2: Local, stmt1: Stmt, stmt2: Stmt, m: SootMethod): Boolean = {
    if (!aliasAnalysisMap.contains(m)) {
      val ug = new CompleteUnitGraph(m.getActiveBody)
      aliasAnalysisMap.addOne(m, new LocalMustAliasAnalysis(ug, false))
    }
    val analysis = aliasAnalysisMap(m)
    analysis.mustAlias(l1, stmt1, l2, stmt2)
  }

  def equalValue(v1: SemanticVal, v2: SemanticVal): Boolean = {
    (v1, v2) match {
      case (SemanticVal.SootValue(sv1, ctx1), SemanticVal.SootValue(sv2, ctx2)) => {
        ctx2.methodEnv.sootMethod == ctx1.methodEnv.sootMethod &&
          {
            (sv1, sv2) match {
              case (l1:Local, l2: Local) =>
                isAlias(l1, l2, ctx2.stmt, ctx1.stmt, ctx1.methodEnv.sootMethod)
              case _ => throw new NotImplementedError()
            }
          }
      }
      case (Name(x), Name(y)) => x == y
      case _ => throw new NotImplementedError()
    }
  }

  def booleanEqualsInt(b: Boolean, i: Int) : Boolean = {
    if (b) { i == 1 } else { i == 0 }
  }
}