package org.uwplse.liquid.analysis

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.spec.AppSpec
import soot.SceneTransformer

class MatchTransformer(val appSpec: AppSpec) extends SceneTransformer {
  var matchedAll: Bindings = _
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    println(Analysis.getConfig)

    if (Analysis.getConfig.scored) {
//      matchedAll = chooseZipMergeR(allMethods, allMethodPatterns, (m: soot.SootMethod, mPat: MethodSignature) => mPat.matchesR(m), scoredBindingTrue(), mergeScoredBinding)
//
//      if (appSpec.classes.nonEmpty) {
//        matchedAll = matchedAll.flatMap(matched => {
//          val ret = chooseZipMergeR[SootClass, ClassSpec, Bindings](
//            classes, appSpec.classes,
//            (c: SootClass, spec: ClassSpec) =>
//              spec.matchesR(config, appSpec, c, matched), scoredBindingsTrue(), mergeScoredBindings)
//          ret.flatten.flatMap(b => mergeBinding(b, matched))
//        })
//      }
    } else {
      matchedAll = appSpec.solve()
    }
  }
}