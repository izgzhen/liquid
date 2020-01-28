package org.uwplse.liquid

import java.util.Collections

import org.apache.commons.io.IOUtils
import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.Match.runOnce
import org.uwplse.liquid.SootInputMode.Java
import org.uwplse.liquid.analysis.ConstantPropTransformer
import soot.jimple.Stmt
import soot.{Scene, SootMethod, Transform}

import scala.jdk.CollectionConverters._

class TestAnalysis extends TestCase {
  @Test def testBasic(): Unit = {
    Analysis.setSootOptions(Java("src/test/resources"))
    soot.Scene.v.loadNecessaryClasses()
    Scene.v.loadClassAndSupport("Test")
    val testClass = Scene.v().getSootClass("Test")
    val classes = List(testClass)
    val config = new Config()
    val ret = runOnce(config, "src/test/resources/test.txt", None, classes)
    assertEquals(
      List(Map("Test" -> "Test", "main" -> "main", "ret" -> "void")),
      ret)
  }

  def prepareTestAnalysis(): SootMethod = {
    Analysis.setSootOptions(Java("src/test/resources"))
    soot.Scene.v.loadNecessaryClasses()
    Scene.v.loadClassAndSupport("Test")
    val testClass = Scene.v.getSootClass("Test")
    val method = testClass.getMethodByName("main3")
    method.retrieveActiveBody
    Scene.v.setEntryPoints(Collections.singletonList(method))
    method
  }

  def testAnalysis(transform: Transform, expectedPath: String, outputPath: String, config: Config): Unit = {
    soot.PackManager.v.getPack("wjtp").add(transform)
    soot.PackManager.v.runPacks()
    
    val p = Runtime.getRuntime.exec("diff " + expectedPath + " " + outputPath)
    val code = p.waitFor()
    val output = IOUtils.toString(p.getInputStream, "UTF-8")
    val errorOutput = IOUtils.toString(p.getErrorStream, "UTF-8")
    assertEquals(output + "\n" + errorOutput, 0, code)
  }

  @Test def testConstantPropAnalysis(): Unit = {
    prepareTestAnalysis()
    val config = new Config()
    val outputPath = "src/test/resources/abstraction.new.txt"
    config.abstractionDumpPath = Some(outputPath)
    val transformer = new ConstantPropTransformer(config)
    testAnalysis(new Transform("wjtp.constantProp", transformer),
      "src/test/resources/abstraction.txt", outputPath, config)
  }

  @Test def testConstantBackPropAnalysis(): Unit = {
    val method = prepareTestAnalysis()
    val sinkStmt = method.getActiveBody.getUnits.asScala.toList(9)
    assert(sinkStmt.toString() == "virtualinvoke $r7.<java.io.PrintStream: void println(java.lang.Object)>(r2)", sinkStmt.toString())

    val config = new Config()
    val outputPath = "src/test/resources/back_abstraction.new.txt"
    config.abstractionDumpPath = Some(outputPath)
    val transformer = new DependencyPropTransformer(sinkStmt.asInstanceOf[Stmt], config)
    testAnalysis(new Transform("wjtp.constantProp", transformer),
      "src/test/resources/back_abstraction.txt", outputPath, config)
    assertEquals(1, transformer.constants.size)
  }
}