package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.Bindings

import scala.collection.mutable

case class AppSpec(patterns: List[PatternDecl], classes: List[ClassSpec], specialConstraints: List[ConstraintDecl]) {
  def solve(): Bindings = {
    val schedule: mutable.Set[Constraint] = mutable.Set()
    schedule.addAll(patterns ++ classes ++ specialConstraints)

    var bindings: Bindings = Bindings.one()
    while (schedule.nonEmpty) {
      val runnables = schedule.filter(_.minSolveCtx().subsetOf(bindings.getKeys))
      val minCostRunnable = runnables.toList.minBy(_.solveCost(bindings.getKeys))
      schedule.remove(minCostRunnable)
      bindings = bindings.map(ctx => minCostRunnable.solve(this, ctx).extend(ctx)).fold(Bindings.Zero()){ case (b1, b2) => b1.sum(b2) }
      if (bindings.isInstanceOf[Bindings.Zero]) {
        return bindings
      }
    }

    bindings
  }
}