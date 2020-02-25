package org.uwplse.liquid

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{FileSystems, Path, StandardWatchEventKinds}

import com.semantic_graph.JsonUtil
import org.uwplse.liquid.analysis.MatchTransformer
import org.uwplse.liquid.spec.SpecParser
import soot.Transform

import scala.io.Source
import scala.jdk.CollectionConverters._

sealed abstract class SootInputMode extends Product with Serializable

object SootInputMode {
  final case class Android(apkPath: String) extends SootInputMode
  final case class Java(classPath: String) extends SootInputMode
}

object Match {
  def runOnce(config: Config, specPath: String, outPath: Option[String]) : List[Map[String, String]] = {
    val text = Source.fromFile(specPath)
    val specParser = new SpecParser()
    val appSpec = specParser.parse(specParser.pAppSpec, text.mkString).get
    text.close()

    val startTime = System.nanoTime()
    val transformer = new MatchTransformer(appSpec)
    soot.PackManager.v.getPack("wjtp").add(new Transform("wjtp.match", transformer))
    soot.PackManager.v.runPacks()
    val spentNanoSeconds = System.nanoTime() - startTime

    val ret = transformer.matchedAll

    if (outPath.isDefined) {
      println("Serializing " + ret.size + " results...")
      val bw = new BufferedWriter(new FileWriter(outPath.get))
      bw.write(JsonUtil.toJson(Map(
        "stats" -> Map(
          "allAppClasses" -> Analysis.getAllAppClasses.size,
          "allMethods" -> Analysis.getAllMethods.size,
          "spentNanoSeconds" -> spentNanoSeconds,
          "analysisStats" -> Analysis.getStats
        ),
        "results" -> ret
      )))
      bw.close()
    }
    ret
  }

  def run(config: Config, specPath: String, outPath: String) : Unit = {
    Analysis.setup(config)

    if (!config.interactive) {
      runOnce(config, specPath, Some(outPath))
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
          runOnce(config, specPath, Some(outPath))
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
