package org.uwplse.liquid

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{FileSystems, Path, StandardWatchEventKinds}

import com.semantic_graph.JsonUtil
import org.uwplse.liquid.analysis.MatchTransformer
import org.uwplse.liquid.spec.{SemanticVal, SpecParser}
import soot.{SootClass, Transform}

import scala.io.Source
import scala.jdk.CollectionConverters._

sealed abstract class SootInputMode extends Product with Serializable

object SootInputMode {
  final case class Android(apkPath: String) extends SootInputMode
  final case class Java(classPath: String) extends SootInputMode
}

object Match {
  def runOnce(config: Config, specPath: String, outPath: Option[String], classes: List[SootClass]) : List[Map[String, String]] = {
    val text = Source.fromFile(specPath)
    val specParser = new SpecParser()
    val appSpec = specParser.parse(specParser.pAppSpec, text.mkString).get
    text.close()

    val transformer = new MatchTransformer(appSpec, classes, config)
    soot.PackManager.v.getPack("wjtp").add(new Transform("wjtp.match", transformer))
    soot.PackManager.v.runPacks()

    val ret = transformer.matchedAll.map(m => m.filter(_._2.isInstanceOf[SemanticVal.Name]).map(p => {
      (p._1, p._2.asInstanceOf[SemanticVal.Name].name)
    }))

    if (outPath.isDefined) {
      println("Serializing results...")
      val bw = new BufferedWriter(new FileWriter(outPath.get))
      bw.write(JsonUtil.toJson(ret))
      bw.close()
    }
    ret
  }


  def run(config: Config, specPath: String, outPath: String) : Unit = {
    val classes = Analysis.setup(config)

    if (!config.interactive) {
      runOnce(config, specPath, Some(outPath), classes)
      return
    }
    val path = FileSystems.getDefault.getPath(specPath)
    val watchedDir = path.getParent
    println("Watching directory " + watchedDir)
    val watchService = FileSystems.getDefault.newWatchService()
    watchedDir.register(watchService, StandardWatchEventKinds.ENTRY_MODIFY);

    println("Ready")
    while (true) {
      val wk = watchService.take()
      for (event <- wk.pollEvents().asScala) {
        val changed = event.context().asInstanceOf[Path]
        println("Changed: " + changed)
        Console.flush()
        if (changed.endsWith(path.getFileName)) {
          println("Running...")
          Console.flush()
          runOnce(config, specPath, Some(outPath), classes)
          println("Finished")
          Console.flush()
        }
      }
      val valid = wk.reset()
      if (!valid) {
        println("Key has been unregistered")
      }
    }
  }
}
