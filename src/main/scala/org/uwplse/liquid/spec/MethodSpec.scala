package org.uwplse.liquid.spec

import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.Utils.OptBindings
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
  def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod): OptBindings = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _: RuntimeException => return Utils.optBindings(false)
    }
    val env = MethodEnv(this, m)
    // TODO: args spec
    name.matches(m.getName) match {
      case Some(nameBinding) =>
        ret.matches(m.getReturnType.toString) match {
          case Some(retBinding) =>
            val bindings = Utils.choose(m.getActiveBody.getUnits.asScala.toList, statements.size).flatMap(chosen => {
              chosen.zip(statements).map({ case (s, spec) =>
                spec.matches(config, appSpec, classSpec, env, s.asInstanceOf[Stmt])
              }).fold(Utils.optBinding(true))(Utils.mergeOptBinding)
            }).toList
            Some(Utils.extend(Utils.extend(bindings, nameBinding), retBinding))
          case None => None
        }
      case None => None
    }
  }
}