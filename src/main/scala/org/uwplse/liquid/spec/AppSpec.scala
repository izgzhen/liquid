package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.Bindings

import scala.collection.mutable

case class AppSpec(patterns: List[PatternDecl], classes: List[ClassSpec], specialConstraints: List[ConstraintDecl]) {
  def isExported(k: String): Boolean = {
    patterns.exists({
      case PatternDecl.MethodSignature(name, _, _, exported) => name == k && exported
    })
  }

  def solve(): Bindings = {
    val schedule: mutable.Set[Constraint] = mutable.Set()
    schedule.addAll(patterns ++ classes ++ specialConstraints)

    var bindings: Bindings = Bindings.one()
    while (schedule.nonEmpty) {
      println("================================================")
      println(s"Bindings: ${bindings.size}")
      println(s"Context keys: ${bindings.getKeys}")
      val runnables = schedule.filter(_.minSolveCtx().subsetOf(bindings.getKeys))
      for (runnable <- runnables) {
        println(s"cost: ${(runnable.solveCost(bindings.getKeys) * runnable.solvedSize(bindings.getKeys))} of $runnable")
      }
      val minCostRunnable = runnables.toList.minBy(c => (c.solveCost(bindings.getKeys) * c.solvedSize(bindings.getKeys)))
      println(s"minCostRunnable: $minCostRunnable")
      schedule.remove(minCostRunnable)
      bindings = bindings.map(ctx => minCostRunnable.solve(this, ctx).extend(ctx)).fold(Bindings.Zero()){ case (b1, b2) => b1.sum(b2) }
      if (bindings.isInstanceOf[Bindings.Zero]) {
        return bindings
      }
    }

    bindings
  }
}