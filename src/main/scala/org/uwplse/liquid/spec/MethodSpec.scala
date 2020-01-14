package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.False
import soot.SootMethod
import soot.jimple.Stmt

import scala.jdk.CollectionConverters._

case class MethodEnv(methodSpec: MethodSpec, sootMethod: SootMethod)

case class MethodSpec(ret: IdentifierPattern, name: IdentifierPattern, statements: List[StatementSpec]) {
  def matches(appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod): Constraint = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _:RuntimeException => return False()
    }
    val c1 = name.matches(m.getName) && ret.matches(m.getReturnType.toString)
    val env = MethodEnv(this, m)
    val matchedStmts = Constraint.foldAnd(statements.map(stmtSpec =>
      Constraint.foldOr(m.getActiveBody.getUnits.asScala.map(unit => stmtSpec.matches(appSpec, classSpec, env, unit.asInstanceOf[Stmt])))))
    c1 && matchedStmts
  }
}