package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.PatternDecl.MethodSignature

case class AppSpec(patterns: List[PatternDecl], classes: List[ClassSpec], constraints: List[ConstraintDecl]) {
  def findMethodSignature(patName: String) : Option[MethodSignature] = {
    for (pat <- patterns) {
      pat match {
        case m@MethodSignature(name, _, _) => if (patName == name) {
          return Some(m)
        }
      }
    }
    None
  }
}
