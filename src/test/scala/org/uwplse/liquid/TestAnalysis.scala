package org.uwplse.liquid

import java.util.Collections

import org.apache.commons.io.IOUtils

import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.Analyze.{runOnce, setSootOptions}
import org.uwplse.liquid.SootInputMode.Java
import org.uwplse.liquid.analysis.ConstantPropTransformer
import soot.{Scene, Transform}

class TestAnalysis extends TestCase {
  @Test def testBasic(): Unit = {
    setSootOptions(Java("src/test/resources"))
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

  @Test def testConstantPropAnalysis(): Unit = {
    setSootOptions(Java("src/test/resources"))
    soot.Scene.v.loadNecessaryClasses()
    Scene.v.loadClassAndSupport("Test")
    val testClass = Scene.v.getSootClass("Test")
    val method = testClass.getMethodByName("main3")
    Scene.v.setEntryPoints(Collections.singletonList(method))
    val config = new Config()
    config.abstractionDumpPath = Some("src/test/resources/abstraction.new.txt")
    val transformer = new ConstantPropTransformer(config)
    soot.PackManager.v.getPack("wjtp").add(new Transform("wjtp.constantProp", transformer))
    soot.PackManager.v.runPacks()
    
    val p = Runtime.getRuntime.exec("diff src/test/resources/abstraction.new.txt src/test/resources/abstraction.txt")
    val code = p.waitFor()
    val output = IOUtils.toString(p.getInputStream, "UTF-8")
    val errorOutput = IOUtils.toString(p.getErrorStream, "UTF-8")
    assertEquals(output + "\n" + errorOutput, 0, code)
  }
}