package org.uwplse.liquid

import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.spec.SemanticVal
import org.uwplse.liquid.spec.Utils._

class TestUtils extends TestCase {
  @Test def testChoose(): Unit = {
    assertEquals(64, choose(List(1,2,3,4), 3).toSet.size)
  }

  @Test def testProd(): Unit = {
    assertEquals(List(List(1, 3), List(1, 4), List(1, 5), List(2, 3), List(2, 4), List(2, 5)),
      prod(List(List(1, 2), List(3, 4, 5))))
  }

  @Test def testChooseZipMerge(): Unit = {
    // FIXME: remove duplicated code
    assertEquals(List(Map(2 -> "2", 3 -> "3")), chooseZipMerge[Int, String, Map[Int, String]](List(1, 2, 3), List("2", "3"), (i: Int, s: String) => {
      if (i.toString == s) { Some(Map(i -> s)) } else { None }
    }, Some(Map()), (z1: Option[Map[Int, String]], z2: Option[Map[Int, String]]) => {
      (z1, z2) match {
        case (Some(l1), Some(l2)) => Some(l1 ++ l2)
        case _ => None
      }
    }))
    assertEquals(List(Map(2 -> "2", 3 -> "3")), chooseZipMerge2[Int, String, Map[Int, String]](List(1, 2, 3), List("2", "3"), (i: Int, s: String) => {
      if (i.toString == s) { Some(Map(i -> s)) } else { None }
    }, Some(Map()), (z1: Option[Map[Int, String]], z2: Option[Map[Int, String]]) => {
      (z1, z2) match {
        case (Some(l1), Some(l2)) => Some(l1 ++ l2)
        case _ => None
      }
    }))
  }

  @Test def testMergeOptBindings(): Unit = {
    val m0 = optBindings(true)
    val m1 = Some(List(Map("a" -> SemanticVal.Name("a"))))
    val m2 = Some(List(Map("a" -> SemanticVal.Name("b"))))
    val m3 = Some(List(Map("b" -> SemanticVal.Name("b"))))
    val m4 = Some(List(Map("a" -> SemanticVal.Name("a"), "b" -> SemanticVal.Name("b"))))
    assertEquals(m1, mergeOptBindings(m0, m1))
    assertEquals(None, mergeOptBindings(m2, m1))
    assertEquals(m4, mergeOptBindings(m3, m1))
  }

  @Test def testMergeOptBinding(): Unit = {
    val m0 = optBinding(true)
    val m1 = Some(Map("a" -> SemanticVal.Name("a")))
    val m2 = Some(Map("a" -> SemanticVal.Name("b")))
    val m3 = Some(Map("b" -> SemanticVal.Name("b")))
    val m4 = Some(Map("a" -> SemanticVal.Name("a"), "b" -> SemanticVal.Name("b")))
    assertEquals(m1, mergeOptBinding(m0, m1))
    assertEquals(None, mergeOptBinding(m2, m1))
    assertEquals(m4, mergeOptBinding(m3, m1))
  }
}