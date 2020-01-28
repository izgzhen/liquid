package org.uwplse.liquid.analysis

import soot.{Value, ValueBox}
import soot.jimple.{IntConstant, StringConstant}
import soot.toolkits.scalar.Pair

import scala.collection.mutable

object Propagator {
  def getTainted(fromValues: Iterable[ValueBox], toValue: Value, sourceValue: Value, sourceAbstractions: Set[ConstVal]) : Set[Pair[Value, Set[ConstVal]]] = {
    val newFlow = mutable.Set[Pair[Value, Set[ConstVal]]]()
    if (toValue != null && fromValues != null) {
      val constants = mutable.Set[ConstVal]()
      for (box <- fromValues) {
        box.getValue match {
          case c: StringConstant => constants.add(ConstVal.StringConstant(c.value))
          case c: IntConstant => constants.add(ConstVal.IntegerConstant(c.value))
          case _ =>
        }
        if (box.getValue == sourceValue) constants.addAll(sourceAbstractions)
      }
      if (!(toValue == sourceValue)) if (constants.nonEmpty) newFlow.add(new Pair[Value, Set[ConstVal]](toValue, constants.toSet))
    }
    newFlow.toSet
  }
}
