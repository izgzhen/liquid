package org.uwplse.liquid

import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.Analyze.{runOnce, setSootOptions}
import org.uwplse.liquid.SootInputMode.Java
import soot.Scene

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
}