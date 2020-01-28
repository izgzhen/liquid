package org.uwplse.liquid

import java.io.{BufferedWriter, FileWriter}
import java.nio.file.{FileSystems, Path, StandardWatchEventKinds}

import com.semantic_graph.JsonUtil
import org.uwplse.liquid.SootInputMode.{Android, Java}
import org.uwplse.liquid.spec.{SemanticVal, SpecParser}
import soot.options.Options
import java.util.Collections

import org.uwplse.liquid.spec.Utils._
import soot.{Scene, SootClass, SootMethod}
import soot.jimple.infoflow.entryPointCreators.DefaultEntryPointCreator

import scala.io.Source
import scala.jdk.CollectionConverters._

sealed abstract class SootInputMode extends Product with Serializable

object SootInputMode {
  final case class Android(apkPath: String) extends SootInputMode
  final case class Java(classPath: String) extends SootInputMode
}


object Analyze {

  private def setEntrypoints(): List[SootClass] = {
    var allMethods = List[SootMethod]()
    var allClasses = List[SootClass]()
    Scene.v().getApplicationClasses.forEach((appCls: SootClass) => {
      if (appCls.isConcrete) {
        Scene.v.forceResolve(appCls.getName, SootClass.BODIES)
        allClasses :+= appCls
        appCls.getMethods.forEach((appMethod: SootMethod) => {
          try {
            appMethod.retrieveActiveBody
            val b = appMethod.getActiveBody
            if (b != null) {
              allMethods :+= appMethod
            }
          } catch {
            case _: RuntimeException =>
          }
        })
      }
    })

    val entryPointCreator = new DefaultEntryPointCreator(allMethods.map(_.getSignature).asJava)
    val dummyMain = entryPointCreator.createDummyMain
    Scene.v.setEntryPoints(Collections.singletonList(dummyMain))
    allClasses
  }


  def runOnce(config: Config, specPath: String, outPath: Option[String], classes: List[SootClass]) : List[Map[String, String]] = {
    val text = Source.fromFile(specPath)
    val specParser = new SpecParser()
    val appSpec = specParser.parse(specParser.pAppSpec, text.mkString).get
    text.close()

    val matchedAll : Bindings = choose(classes, appSpec.classes.size).flatMap(chosen => {
      chosen.zip(appSpec.classes).map({ case (c, spec) =>
        spec.matches(config, appSpec, c)
      }).fold(optBindings(true))(mergeOptBindings)
    }).toList.flatten

    val ret = matchedAll.map(m => m.filter(_._2.isInstanceOf[SemanticVal.Name]).map(p => {
      (p._1, p._2.asInstanceOf[SemanticVal.Name].name)
    }))

    // soot.PackManager.v.runPacks()

    if (outPath.isDefined) {
      println("Serializing results...")
      val bw = new BufferedWriter(new FileWriter(outPath.get))
      bw.write(JsonUtil.toJson(ret))
      bw.close()
    }
    ret
  }

  def setSootOptions(mode: SootInputMode) : Unit = {
    mode match {
      case Android(apkPath) =>
        Options.v.set_process_dir(Collections.singletonList(apkPath))
        Options.v.set_android_jars("android-platforms/")
        Options.v.set_soot_classpath("android-platforms/android-28/android.jar")
        Options.v.set_src_prec(Options.src_prec_apk)
        Options.v.set_process_multiple_dex(true)
      case Java(classPath) =>
        Options.v.set_process_dir(Collections.singletonList(classPath))
        Options.v.set_soot_classpath(classPath)
        Options.v.set_src_prec(Options.src_prec_class)
        Options.v.set_keep_line_number(true)
    }
    Options.v.set_whole_program(true)
    Options.v.set_allow_phantom_refs(true)
    Options.v.set_ignore_resolution_errors(true)
    Options.v.set_no_writeout_body_releasing(true)
    Options.v.set_output_format(Options.output_format_none)
    Options.v.setPhaseOption("cg.spark", "on")
    Options.v.set_no_bodies_for_excluded(true)
    Options.v.set_omit_excepting_unit_edges(true)
  }

  def run(config: Config, apkPath: String, specPath: String, outPath: String) : Unit = {
    soot.G.reset()
    setSootOptions(Android(apkPath))
    soot.Scene.v.loadNecessaryClasses()

    val classes = setEntrypoints()

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
