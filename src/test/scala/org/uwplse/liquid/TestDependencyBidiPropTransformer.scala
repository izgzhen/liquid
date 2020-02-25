package org.uwplse.liquid

import soot.jimple.Stmt
import soot.{SceneTransformer, Value}

class TestDependencyBidiPropTransformer(val sinkStmt: Stmt, val config: Config) extends SceneTransformer {
  var constants: Set[Value] = Set()
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    constants = Analysis.dependencyPropAnalysis2(sinkStmt, config.abstractionDumpPath)
  }
}