package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils._

sealed abstract class IdentifierPattern extends Product with Serializable {
  def matches(s: String) : OptBinding
}

object IdentifierPattern {
  final case class NamedWildcard(binder: String) extends IdentifierPattern {
    override def matches(s: String): OptBinding = Some(Map(binder -> SemanticVal.Name(s)))
  }
  final case class StringIdentifier(str: String) extends IdentifierPattern {
    override def matches(s: String): OptBinding = optBinding(s == str)
  }
}