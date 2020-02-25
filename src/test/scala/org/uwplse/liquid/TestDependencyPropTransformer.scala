package org.uwplse.liquid

import soot.{SceneTransformer, SootMethod, Value}
import soot.jimple.Stmt

class TestDependencyPropTransformer(val sinkMethod: SootMethod, val sinkStmt: Stmt, val config: Config) extends SceneTransformer {
  var constants: Set[Value] = Set()
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    constants = Analysis.dependencyPropAnalysis(sinkMethod, sinkStmt, config.abstractionDumpPath)
  }
}