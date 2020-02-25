package org.uwplse.liquid.spec

import org.uwplse.liquid.analysis.{Binding, Bindings}
import org.uwplse.liquid.spec.Utils._
import org.uwplse.liquid.Analysis
import org.uwplse.liquid.spec.IdentifierPattern.NamedWildcard
import soot.SootClass

import scala.jdk.CollectionConverters._

case class ClassSpec(name: IdentifierPattern, parent: Option[IdentifierPattern],
                     methods: List[MethodSpec]) extends Constraint {

  override def solve(appSpec: AppSpec, ctx: Binding): Bindings = {
    if (Analysis.getAllAppClasses.isEmpty) {
      Bindings.one()
    } else {
      name match {
        case NamedWildcard(binder) =>
          ctx.m.get(binder) match {
            case Some(value) =>
              val c = soot.Scene.v.getSootClass(value.asInstanceOf[ConcreteVal.Name].name)
              return matches(appSpec, c, ctx)
            case None =>
          }
        case _ =>
      }
      Analysis.getAllAppClasses.map { c =>
        matches(appSpec, c, ctx)
      }.fold(Bindings.Zero()){ case (b1, b2) => b1.sum(b2) }
    }
  }

  override def solveCost(ctx: Set[String]): Long = {
    name match {
      case NamedWildcard(binder) =>
        if (ctx.contains(binder)) {
          methods.map(m => m.solveCost(ctx)).sum
        } else {
          Analysis.getAllAppClasses.map(c => c.getMethods.size().toLong * methods.map(_.solveCost(ctx)).sum).sum
        }
      case IdentifierPattern.StringIdentifier(_) => 1
    }
  }

  override def solvedSize(ctx: Set[String]): Long = {
    val classFactor = if (name.isInstanceOf[NamedWildcard]) { Analysis.getAllAppClasses.size } else { 1 }
    classFactor * methods.size
  }

  def matches(appSpec: AppSpec, cls: SootClass, ctx: Binding) : Bindings = {
    val filteredOut = parent match {
      case Some(NamedWildcard("packageNotWhitelisted")) =>
        Analysis.getConfig.whitelistPackagePrefixes.asScala.exists(cls.getName.startsWith)
      case Some(NamedWildcard("adClass")) =>
        Analysis.getConfig.adPackagePrefixes.asScala.exists(cls.getName.startsWith)
      case _ => false
    }
    if (filteredOut) {
      return Bindings.Zero()
    }

    name.matches(cls.getName) match {
      case Some(nameBinding) =>
        val matchedParent = if (parent.isDefined) {
          optBinding(Analysis.isSubClass(cls, parent.get))
        } else {
          optBinding(true)
        }
        matchedParent match {
          case Some(parentBinding) =>
            val bs = Bindings.from({
              val choices = choose(cls.getMethods.asScala.toList, methods.size)
              choices.zipWithIndex.flatMap { case (chosen, idx) =>
                assert(chosen.size == methods.size)
//                println(s"Matching method #${idx}/${choices.size}")
                chosen.zip(methods).map({
                  case (m, spec) => spec.matches(appSpec, this, m, ctx)
                }).fold(Bindings.Zero()){ case (x,y) => x.sum(y) }
              }
            })
            bs.extend(nameBinding).extend(parentBinding)
          case _ => Bindings.Zero()
        }
      case None => Bindings.Zero()
    }
  }

//  def matchesR(config: Config, appSpec: AppSpec, cls: SootClass, ctx: Binding) : ScoredBindings = {
//    val filteredOut = parent match {
//      case Some(NamedWildcard("packageNotWhitelisted")) =>
//        config.whitelistPackagePrefixes.asScala.exists(cls.getName.startsWith)
//      case _ => false
//    }
//    if (filteredOut) {
//      return scoredBindingsFalse()
//    }
//
//    name.matchesR(cls.getName) match {
//      case (nameBinding, nameBindingScore) =>
//        val (parentBinding, parentBindingScore) = if (parent.isDefined) {
//          if (cls.hasSuperclass || cls.getInterfaceCount > 0) {
//            if (cls.hasSuperclass) {
//              parent.get.matchesR(cls.getSuperclass.getName)
//            } else {
//              val matches = cls.getInterfaces.asScala.map(i => parent.get.matchesR(i.getName))
//              matches.maxBy(_._2)
//            }
//          } else {
//            scoredBindingFalse()
//          }
//        } else {
//          scoredBindingFalse()
//        }
//        val (bs, score) = choose(cls.getMethods.asScala.toList, methods.size).map(chosen => {
//          chosen.zip(methods).map({ case (m, spec) =>
//            spec.matchesR(config, appSpec, this, m, ctx)
//          }).fold(scoredBindingsTrue())(mergeScoredBindings)
//        }).fold(scoredBindingsTrue())(extendScoredBindings)
//        (extend(extend(bs, nameBinding), parentBinding), score * parentBindingScore * nameBindingScore)
//    }
//  }
  /**
   * Minimum set of variables in ctx required to solve, otherwise calling this will abort
   *
   * @return
   */
  override def minSolveCtx(): Set[String] = Set()
}