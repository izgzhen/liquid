package org.uwplse.liquid.spec

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.analysis.{Binding, Bindings}
import org.uwplse.liquid.spec.IdentifierPattern.{NamedWildcard, StringIdentifier}
import org.uwplse.liquid.spec.Utils._
import soot.SootMethod

sealed abstract class PatternDecl extends Product with Serializable with Constraint

object PatternDecl {
  final case class MethodSignature(name: String, classId: IdentifierPattern,
                                   methodId: IdentifierPattern, exported: Boolean) extends PatternDecl {
    def solve(appSpec: AppSpec, ctx: Binding): Bindings = {
      ctx.m.get(name) match {
        case Some(value) => {
          val binding = matches(value.asInstanceOf[SemanticVal.Method].m)
          if (binding.isDefined) {
            Bindings.NonEmpty(binding.get, List())
          } else {
            Bindings.Zero()
          }
        }
        case None => Bindings.from(Analysis.getAllMethods.flatMap(m => matches(m)))
      }
    }

    def solvedSize(ctx: Set[String]): Long = {
      (classId, methodId) match {
        case (NamedWildcard(_), NamedWildcard(_)) => Analysis.getAllMethods.size.toLong
        case (StringIdentifier(_), StringIdentifier(_)) => 1
        case _ => Analysis.getAllMethods.size.toLong / Analysis.getAllClasses.size.toLong
      }
    }

    def solveCost(ctx: Set[String]): Long = {
      if (ctx.contains(name)) { 1 } else Analysis.getAllMethods.size.toLong
    }

    def matches(m: SootMethod): OptBinding = {
      val className = m.getDeclaringClass.getName
      mergeOptBinding(classId.matches(className), methodId.matches(m.getName)) match {
        case Some(binding) => binding.prod(Binding(Map(name -> SemanticVal.Method(m))))
        case None => None
      }
    }

//    def matchesR(m: SootMethod): ScoredBinding = {
//      val className = m.getDeclaringClass.getName
//      mergeScoredBinding(classId.matchesR(className), methodId.matchesR(m.getName)) match {
//        case (binding, score) =>
//          (binding ++ Map(name -> SemanticVal.Method(m)), score)
//      }
//    }
    /**
     * Minimum set of variables in ctx required to solve, otherwise calling this will abort
     *
     * @return
     */
    override def minSolveCtx(): Set[String] = Set()
  }
}
