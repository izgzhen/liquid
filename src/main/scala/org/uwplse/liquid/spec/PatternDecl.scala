package org.uwplse.liquid.spec

import org.uwplse.liquid.spec.Utils._
import soot.SootMethod

sealed abstract class PatternDecl extends Product with Serializable

object PatternDecl {
  final case class MethodSignature(name: String, classId: IdentifierPattern, methodId: IdentifierPattern) extends PatternDecl {
    def matches(m: SootMethod): OptBinding = {
      val className = m.getDeclaringClass.getName
      mergeOptBinding(classId.matches(className), methodId.matches(m.getName)) match {
        case Some(binding) => {
          mergeBinding(binding, Map(name -> SemanticVal.Method(m)))
        }
        case None => None
      }
    }

    def matchesR(m: SootMethod): ScoredBinding = {
      val className = m.getDeclaringClass.getName
      mergeScoredBinding(classId.matchesR(className), methodId.matchesR(m.getName)) match {
        case (binding, score) =>
          (binding ++ Map(name -> SemanticVal.Method(m)), score)
      }
    }
  }
}
