package org.uwplse.liquid

import soot.{SceneTransformer, Value}
import soot.jimple.Stmt

class DependencyPropTransformer(val sinkStmt: Stmt, val config: Config) extends SceneTransformer {
  var constants: Set[Value] = Set()
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    constants = Analysis.dependencyPropAnalysis(sinkStmt, config.abstractionDumpPath)
  }
}