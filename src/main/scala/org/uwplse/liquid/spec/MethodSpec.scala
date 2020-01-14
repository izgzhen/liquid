package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.False
import soot.SootMethod
import soot.jimple.Stmt

import scala.jdk.CollectionConverters._

case class MethodEnv(methodSpec: MethodSpec, sootMethod: SootMethod)

/**
 * Method specification
 * @param ret return type id pattern
 * @param name name id pattern
 * @param locals a map from local variable name to its type id
 * @param statements a list of statement specifications
 */
case class MethodSpec(ret: IdentifierPattern, name: IdentifierPattern,
                      locals: Map[String, String], statements: List[StatementSpec]) {
  def matches(appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod): Constraint = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _:RuntimeException => return False()
    }
    val c1 = name.matches(m.getName) && ret.matches(m.getReturnType.toString)
    val env = MethodEnv(this, m)
    // TODO: improve the matching logic here
    val matchedStmts = Constraint.foldAnd(statements.map(stmtSpec =>
      Constraint.foldOr(m.getActiveBody.getUnits.asScala.map(unit => stmtSpec.matches(appSpec, classSpec, env, unit.asInstanceOf[Stmt])))))
    c1 && matchedStmts
  }
}