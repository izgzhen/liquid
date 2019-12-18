package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Constraint.{NameEquals}

sealed abstract class IdentifierPattern extends Product with Serializable {
  def matches(s: String) : Constraint
}

object IdentifierPattern {
  final case class NamedWildcard(binder: String) extends IdentifierPattern {
    override def matches(s: String): Constraint = NameEquals(binder, s)
  }
  final case class StringIdentifier(str: String) extends IdentifierPattern {
    override def matches(s: String): Constraint = Constraint.from(s == str)
  }
}