package org.uwplse.liquid.analysis

import soot.Value
import soot.ValueBox
import soot.jimple.IntConstant
import soot.jimple.StringConstant
import soot.toolkits.scalar.Pair
import scala.collection.mutable

object Propagator {
  def getTainted(fromValues: Iterable[ValueBox], toValue: Value, sourceValue: Value, sourceAbstractions: Set[Abstraction]) : Set[Pair[Value, Set[Abstraction]]] = {
    val newFlow = mutable.Set[Pair[Value, Set[Abstraction]]]()
    if (toValue != null && fromValues != null) {
      val constants = mutable.Set[Abstraction]()
      for (box <- fromValues) {
        box.getValue match {
          case c: StringConstant => constants.add(Abstraction.StringConstant(c.value))
          case c: IntConstant => constants.add(Abstraction.IntegerConstant(c.value))
          case _ =>
        }
        if (box.getValue == sourceValue) constants.addAll(sourceAbstractions)
      }
      if (!(toValue == sourceValue)) if (constants.nonEmpty) newFlow.add(new Pair[Value, Set[Abstraction]](toValue, constants.toSet))
    }
    newFlow.toSet
  }
}
