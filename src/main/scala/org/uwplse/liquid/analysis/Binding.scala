package org.uwplse.liquid.analysis

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.spec.ConcreteVal

/* Created at 2/15/20 by zhen */
case class Binding(m: Map[String, ConcreteVal]) {
  def sum(b2: Binding): Bindings = {
    assert (b2.m.keySet == m.keySet)
    Bindings.NonEmpty(this, List(b2))
  }
  def prod(b2: Binding): Option[Binding] = {
    val common = m.keySet intersect b2.m.keySet
    if (common.nonEmpty && !common.forall(k => Analysis.equalValue(m(k), b2.m(k)))) {
      None
    } else {
      Some(Binding(m ++ b2.m))
    }
  }
}

object Binding {
  def one(): Binding = Binding(Map())
}