package org.uwplse.liquid.spec

import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.Utils._
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
  def matches(config: Config, appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod, ctx: Binding): OptBindings = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _: RuntimeException => return optBindings(false)
    }
    val env = MethodEnv(this, m)
    // TODO: args spec
    name.matches(m.getName) match {
      case Some(nameBinding) =>
        ret.matches(m.getReturnType.toString) match {
          case Some(retBinding) =>
            val bindings = if (statements.isEmpty) {
              optBindings(true).get
            } else {
              choose(m.getActiveBody.getUnits.asScala.toList, statements.size).flatMap(chosen => {
                chosen.zip(statements).map({ case (s, spec) =>
                  spec.matches(config, appSpec, classSpec, env, s.asInstanceOf[Stmt], ctx)
                }).fold(optBinding(true))(mergeOptBinding)
              }).toList
            }
            Some(extend(extend(bindings, nameBinding), retBinding))
          case None => None
        }
      case None => None
    }
  }

  def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod, ctx: Binding): ScoredBindings = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _: RuntimeException => return scoredBindingsFalse()
    }
    val env = MethodEnv(this, m)
    // TODO: args spec
    name.matchesR(m.getName) match {
      case (nameBinding, nameBindingScore) =>
        ret.matchesR(m.getReturnType.toString) match {
          case (retBinding, retBindingScore) =>
            val (bindings, score) = if (statements.isEmpty) {
              scoredBindingsTrue()
            } else {
              choose(m.getActiveBody.getUnits.asScala.toList, statements.size).map(chosen => {
                val (b, s) = chosen.zip(statements).map({ case (s, spec) =>
                  spec.matchesR(config, appSpec, classSpec, env, s.asInstanceOf[Stmt], ctx)
                }).fold(scoredBindingTrue())(mergeScoredBinding)
                (List(b), s)
              }).fold(scoredBindingsTrue())(extendScoredBindings)
            }
            (extend(extend(bindings, nameBinding), retBinding), score * retBindingScore * nameBindingScore)
        }
    }
  }
}