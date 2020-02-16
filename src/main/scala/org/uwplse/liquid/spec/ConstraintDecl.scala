package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.analysis.{Binding, Bindings}

/* Created at 2/2/20 by zhen */
/**
 *
 * @param name
 * @param argNames TODO: relax to [[IdentifierPattern]]
 */
case class ConstraintDecl(name: String, argNames: List[String]) extends Constraint {
  def solve(appSpec: AppSpec, ctx: Binding): Bindings = {
    val argValues = argNames.map(x => ctx.m(x))
    name match {
      case "reachable" if argValues.size == 2 =>
        (argValues(0), argValues(1)) match {
          case (SemanticVal.Method(m1), SemanticVal.Method(m2)) => {
            if (Analysis.isReachable(m1, m2)) {
              return Bindings.one()
            } else {
              return Bindings.Zero()
            }
          }
          case _ =>
        }
      case _ =>
    }
    throw new RuntimeException(this + "\n" + ctx)
  }

  override def minSolveCtx(): Set[String] = argNames.toSet

  /**
   * Map query will become constant overtime
   * @return
   */
  override def solveCost(ctx: Set[String]): Int = 1

  override def solvedSize(ctx: Set[String]): Int = 1
}
