package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis

import scala.jdk.CollectionConverters._

/* Created at 2/2/20 by zhen */
/**
 *
 * @param name
 * @param argNames TODO: relax to [[IdentifierPattern]]
 */
case class ConstraintDecl(name: String, argNames: List[String]) {
  def satisfies(binding: Map[String, SemanticVal], appSpec: AppSpec): Boolean = {
    val argValues = argNames.map(x => binding(x))
    name match {
      case "reachable" if argValues.size == 2 =>
        (argValues(0), argValues(1)) match {
          case (SemanticVal.Method(m1), SemanticVal.Method(m2)) => {
            return Analysis.isReachable(m1, m2)
          }
          case _ =>
        }
      case _ =>
    }
    throw new RuntimeException(this.toString + "\n" + binding.toString)
  }
}
