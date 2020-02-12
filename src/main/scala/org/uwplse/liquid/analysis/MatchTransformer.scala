package org.uwplse.liquid.analysis

import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.{AppSpec, ClassSpec}
import org.uwplse.liquid.spec.PatternDecl.MethodSignature
import org.uwplse.liquid.spec.Utils._
import soot.{SceneTransformer, SootClass}

import scala.jdk.CollectionConverters._

class MatchTransformer(val appSpec: AppSpec, val classes: List[SootClass], val config: Config) extends SceneTransformer {
  var matchedAll: Bindings = List()
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    val allMethods = soot.Scene.v.getClasses.asScala.flatMap(_.getMethods.asScala).toList
    val allMethodPatterns = appSpec.getMethodPatterns
    if (config.scored) {
      matchedAll = chooseZipMergeR(allMethods, allMethodPatterns, (m: soot.SootMethod, mPat: MethodSignature) => mPat.matchesR(m), scoredBindingTrue(), mergeScoredBinding)

      if (appSpec.classes.nonEmpty) {
        matchedAll = matchedAll.flatMap(matched => {
          val ret = chooseZipMergeR[SootClass, ClassSpec, Bindings](
            classes, appSpec.classes,
            (c: SootClass, spec: ClassSpec) =>
              spec.matchesR(config, appSpec, c, matched), scoredBindingsTrue(), mergeScoredBindings)
          ret.flatten.flatMap(b => mergeBinding(b, matched))
        })
      }
    } else {
      matchedAll = chooseZipMerge2(allMethods, allMethodPatterns, (m: soot.SootMethod, mPat: MethodSignature) => mPat.matches(m), optBinding(true), mergeOptBinding)

      if (appSpec.classes.nonEmpty) {
        matchedAll = matchedAll.flatMap(matched => {
          chooseZipMerge2(classes, appSpec.classes, (c: SootClass, spec: ClassSpec) => spec.matches(config, appSpec, c, matched), optBindings(true), mergeOptBindings)
            .flatten.flatMap(b => mergeBinding(b, matched))
        })
      }
    }

    for (constraint <- appSpec.constraints) {
      matchedAll = matchedAll.filter(binding => {
        constraint.satisfies(binding, appSpec)
      })
    }
  }
}