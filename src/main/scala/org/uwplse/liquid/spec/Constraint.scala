package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.{Binding, Bindings}

/* Created at 2/15/20 by zhen */
trait Constraint {
  /**
   * Return new bindings under ctx
   * @param appSpec
   * @param ctx
   * @return
   */
  def solve(appSpec: AppSpec, ctx: Binding): Bindings
  def solveCost(ctx: Set[String]): Int
  def solvedSize(ctx: Set[String]): Int

  /**
   * Minimum set of variables in ctx required to solve, otherwise calling this will abort
   * @return
   */
  def minSolveCtx(): Set[String]
}