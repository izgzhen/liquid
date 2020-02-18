package org.uwplse.liquid.analysis

import org.uwplse.liquid.Analysis
import org.uwplse.liquid.spec.{AppSpec, ConcreteVal}
import soot.SceneTransformer

class MatchTransformer(val appSpec: AppSpec) extends SceneTransformer {
  var matchedAll: List[Map[String, String]] = _
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
      val bindings = appSpec.solve()
      matchedAll = bindings.map(b => {
        b.m.flatMap {
          case (k, ConcreteVal.Method(m)) =>
            if (appSpec.isExported(k)) {
              Some((k, m.getSignature))
            } else {
              None
            }
          case (k, ConcreteVal.Name(v)) => Some((k, v))
          case _ => None
        }
      }).toList
    }
  }
}