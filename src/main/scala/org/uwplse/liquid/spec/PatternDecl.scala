package org.uwplse.liquid.spec

import soot.SootMethod

sealed abstract class PatternDecl extends Product with Serializable

object PatternDecl {
  final case class MethodSignature(name: String, classId: IdentifierPattern, methodId: IdentifierPattern) extends PatternDecl {
    def matches(m: SootMethod): Constraint = {
      val className = m.getDeclaringClass.getName
      classId.matches(className) && methodId.matches(m.getName)
    }
  }
}
