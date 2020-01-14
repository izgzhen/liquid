package org.uwplse.liquid
import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.spec.Arguments
import org.uwplse.liquid.spec.Expr.LitExpr
import org.uwplse.liquid.spec.IdentifierPattern._
import org.uwplse.liquid.spec.Literal.{BoolLit, IntLit, StringLit}
import org.uwplse.liquid.spec.PatternDecl.MethodSignature
import org.uwplse.liquid.spec.StatementSpec.Invoke
import org.uwplse.liquid.spec.{ClassSpec, MethodSpec, SpecParser}

class TestParser extends TestCase {
  @Test def testParseIdentifier(): Unit = {
    val parser = new SpecParser()
    assertEquals(NamedWildcard("H"), parser.parse(parser.pId, "_H").get)
    assertEquals(StringIdentifier("exec"), parser.parse(parser.pId, """exec""").get)
    assertEquals(StringIdentifier("<init>"), parser.parse(parser.pId, """<init>""").get)
    assertEquals(StringIdentifier("org.apache.http.client.HttpClient"),
      parser.parse(parser.pId, """org.apache.http.client.HttpClient""").get)
  }

  @Test def testParseMethodSig(): Unit = {
    val parser = new SpecParser()
    assertEquals(MethodSignature("exec", NamedWildcard("execClass"), StringIdentifier("exec")),
      parser.parse(parser.pDecl, """methodSig exec(_execClass, exec)""").get)
  }

  @Test def testParseLiteral(): Unit = {
    val parser = new SpecParser()
    assertEquals(IntLit(0), parser.parse(parser.pLiteral, "0").get)
    assertEquals(BoolLit(false), parser.parse(parser.pLiteral, "false").get)
  }

  @Test def testParseStmt(): Unit = {
    val parser = new SpecParser()
    assertEquals(Invoke("exec", Arguments.Contain(Set())),  parser.parse(parser.pStmt, """exec(...);""").get)
    assertEquals(Invoke("setIntentData", Arguments.Are(List(LitExpr(StringLit("market://details?id=com.great.animalpop"))))),
      parser.parse(parser.pStmt, """setIntentData("market://details?id=com.great.animalpop");""").get)
  }

  @Test def testParseMethod(): Unit = {
    val parser = new SpecParser()
    assertEquals(MethodSpec(NamedWildcard("_r0"), NamedWildcard("f"), Map(), List(Invoke("exec", Arguments.Contain(Set())))),
      parser.parse(parser.pMethodSpec,
        """_ _f(...) {
          |  exec(...);
          |}""".stripMargin).get)
    assertEquals(MethodSpec(NamedWildcard("ret"), StringIdentifier("onClick"), Map(), List()),
      parser.parse(parser.pMethodSpec,
        """_ret onClick(...) {
          |}""".stripMargin).get)

    // FIXME: "_ f(...) {}" doesn't work
  }

  @Test def testParseParentClass(): Unit = {
    val parser = new SpecParser()
    assertEquals(None, parser.parse(parser.pParentClass, "").get)
    assertEquals(Some(StringIdentifier("android.content.Context")),
      parser.parse(parser.pParentClass, """(android.content.Context)""").get)
  }

  @Test def testParseClass(): Unit = {
    val parser = new SpecParser()
    assertEquals(ClassSpec(NamedWildcard("H"), None,
      List(MethodSpec(NamedWildcard("_r0"), NamedWildcard("f"), Map(), List(
        Invoke("exec", Arguments.Contain(Set())),
        Invoke("setIntentPackage", Arguments.Are(List(LitExpr(StringLit("com.android.vending"))))))))),
      parser.parse(parser.pClassSpec,
        """class _H {
          |  _ _f(...) {
          |    exec(...);
          |    setIntentPackage("com.android.vending");
          |  }
          |}""".stripMargin).get)

    assertEquals(ClassSpec(NamedWildcard("_r1"), Some(StringIdentifier("android.content.DialogInterface.OnClickListener")), List()),
      parser.parse(parser.pClassSpec,
        """class _(android.content.DialogInterface.OnClickListener) {
          |}""".stripMargin).get)
  }
}
