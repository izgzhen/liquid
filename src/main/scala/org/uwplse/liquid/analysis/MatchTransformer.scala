package org.uwplse.liquid.analysis

import org.uwplse.liquid.Config
import org.uwplse.liquid.spec.AppSpec
import org.uwplse.liquid.spec.Utils.{Bindings, choose, mergeOptBindings, optBindings}
import soot.{SceneTransformer, SootClass}

class MatchTransformer(val appSpec: AppSpec, val classes: List[SootClass], val config: Config) extends SceneTransformer {
  var matchedAll: Bindings = List()
  override protected def internalTransform(phaseName: String, map: java.util.Map[String, String]): Unit = {
    matchedAll = choose(classes, appSpec.classes.size).flatMap(chosen => {
      chosen.zip(appSpec.classes).map({ case (c, spec) =>
        spec.matches(config, appSpec, c)
      }).fold(optBindings(true))(mergeOptBindings)
    }).toList.flatten
  }
}