package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils._
import soot.SootMethod

sealed abstract class PatternDecl extends Product with Serializable

object PatternDecl {
  final case class MethodSignature(name: String, classId: IdentifierPattern, methodId: IdentifierPattern) extends PatternDecl {
    def matches(m: SootMethod): OptBinding = {
      val className = m.getDeclaringClass.getName
      mergeOptBinding(classId.matches(className), methodId.matches(m.getName))
    }
  }
}
