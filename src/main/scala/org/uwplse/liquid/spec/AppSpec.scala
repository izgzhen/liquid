package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.PatternDecl.MethodSignature

case class AppSpec(patterns: List[PatternDecl], classes: List[ClassSpec], constraints: List[ConstraintDecl]) {
  def getMethodPatterns: List[MethodSignature] = {
    patterns.filter(_.isInstanceOf[MethodSignature]).map(_.asInstanceOf[MethodSignature])
  }
}
