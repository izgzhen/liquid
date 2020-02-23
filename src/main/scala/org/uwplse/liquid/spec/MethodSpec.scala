package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.{Binding, Bindings}
import soot.SootMethod

case class MethodEnv(methodSpec: MethodSpec, sootMethod: SootMethod)

/**
 * Method specification
 * @param ret return type id pattern
 * @param name name id pattern
 * @param locals a map from local variable name to its type id
 * @param bodySpec body specification
 */
case class MethodSpec(ret: IdentifierPattern, name: IdentifierPattern,
                      locals: Map[String, String], bodySpec: BodySpec) {

  def solveCost(ctx: Set[String]): Long = bodySpec.solveCost(ctx)

  def matches(appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod, ctx: Binding): Bindings = {
    try {
      m.retrieveActiveBody()
    } catch {
      case _: RuntimeException => return Bindings.Zero()
    }
    val env = MethodEnv(this, m)
    // TODO: args spec
    name.matches(m.getName) match {
      case Some(nameBinding) =>
        ret.matches(m.getReturnType.toString) match {
          case Some(retBinding) =>
            val bindings = bodySpec.matches(appSpec, classSpec, env, m.getActiveBody, ctx)
            bindings.extend(nameBinding).extend(retBinding)
          case None => Bindings.Zero()
        }
      case None => Bindings.Zero()
    }
  }

//  def matchesR(config: Config, appSpec: AppSpec, classSpec: ClassSpec, m: SootMethod, ctx: Binding): ScoredBindings = {
//    try {
//      m.retrieveActiveBody()
//    } catch {
//      case _: RuntimeException => return scoredBindingsFalse()
//    }
//    val env = MethodEnv(this, m)
//    // TODO: args spec
//    name.matchesR(m.getName) match {
//      case (nameBinding, nameBindingScore) =>
//        ret.matchesR(m.getReturnType.toString) match {
//          case (retBinding, retBindingScore) =>
//            val (bindings, score) = if (statements.isEmpty) {
//              scoredBindingsTrue()
//            } else {
//              choose(m.getActiveBody.getUnits.asScala.toList, statements.size).map(chosen => {
//                val (b, s) = chosen.zip(statements).map({ case (s, spec) =>
//                  spec.matchesR(config, appSpec, classSpec, env, s.asInstanceOf[Stmt], ctx)
//                }).fold(scoredBindingTrue())(mergeScoredBinding)
//                (List(b), s)
//              }).fold(scoredBindingsTrue())(extendScoredBindings)
//            }
//            (extend(extend(bindings, nameBinding), retBinding), score * retBindingScore * nameBindingScore)
//        }
//    }
//  }
}