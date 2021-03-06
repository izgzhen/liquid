package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.Binding
import org.uwplse.liquid.spec.Utils._

sealed abstract class IdentifierPattern extends Product with Serializable {
  def matches(s: String) : OptBinding
//  def matchesR(s: String) : ScoredBinding = scoreOptBinding(matches(s))
  def fromBinding(b: Binding): ConcreteVal
}

object IdentifierPattern {
  final case class NamedWildcard(binder: String) extends IdentifierPattern {
    def matches(s: String): OptBinding = Some(Binding(Map(binder -> ConcreteVal.Name(s))))
    def fromBinding(b: Binding): ConcreteVal = b.m(binder)
  }
  final case class StringIdentifier(str: String) extends IdentifierPattern {
    def matches(s: String): OptBinding = optBinding(s == str)
    def fromBinding(b: Binding): ConcreteVal = ConcreteVal.Name(str)
  }
}