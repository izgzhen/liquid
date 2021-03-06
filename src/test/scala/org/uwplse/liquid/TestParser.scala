package org.uwplse.liquid
import junit.framework.TestCase
import org.junit.Assert._
import org.junit.Test
import org.uwplse.liquid.spec.{Arguments, BodySpec, ClassSpec, ConstraintDecl, MethodSpec, SpecParser}
import org.uwplse.liquid.spec.Expr.{LitExpr, VarExpr}
import org.uwplse.liquid.spec.IdentifierPattern._
import org.uwplse.liquid.spec.Literal.{BoolLit, IntLit, RegexLit, StringLit}
import org.uwplse.liquid.spec.PatternDecl.MethodSignature
import org.uwplse.liquid.spec.StatementSpec.Invoke

class TestParser extends TestCase {
  @Test def testParseIdentifier(): Unit = {
    val parser = new SpecParser()
    assertEquals(NamedWildcard("H"), parser.parse(parser.pId, "_H").get)
    assertEquals(NamedWildcard("H2"), parser.parse(parser.pId, "_H2").get)
    assertEquals(StringIdentifier("exec"), parser.parse(parser.pId, """exec""").get)
    assertEquals(StringIdentifier("<init>"), parser.parse(parser.pId, """<init>""").get)
    assertEquals(StringIdentifier("com.revlwp.wallpaper.newlp.MainActivity$16$1$3"), parser.parse(parser.pId, """com.revlwp.wallpaper.newlp.MainActivity$16$1$3""").get)
    assertEquals(StringIdentifier("org.apache.http.client.HttpClient"),
      parser.parse(parser.pId, """org.apache.http.client.HttpClient""").get)
  }

  @Test def testParsePatternDecl(): Unit = {
    val parser = new SpecParser()
    assertEquals(MethodSignature("exec", NamedWildcard("execClass"), StringIdentifier("exec"), exported = false),
      parser.parse(parser.pDecl, """methodSig exec(_execClass, exec)""").get)
    assertEquals(MethodSignature("exec", NamedWildcard("execClass"), StringIdentifier("exec"), exported = true),
      parser.parse(parser.pDecl, """exported methodSig exec(_execClass, exec)""").get)
  }

  @Test def testParseLocalVarDecl(): Unit = {
    val parser = new SpecParser()
    assertEquals(("arr", "byte[]"), parser.parse(parser.pLocalVarDecl, "byte[] arr;").get)
  }

  @Test def testParseLiteral(): Unit = {
    val parser = new SpecParser()
    assertEquals(IntLit(0), parser.parse(parser.pLiteral, "0").get)
    assertEquals(BoolLit(false), parser.parse(parser.pLiteral, "false").get)
  }

  @Test def testParseStmt(): Unit = {
    val parser = new SpecParser()
    assertEquals(Invoke("readInputStream", Arguments.Are(List(VarExpr("fi"), VarExpr("arr"))), Some("read")),
      parser.parse(parser.pStmt, """read = readInputStream(fi, arr);""").get)
    assertEquals(Invoke("exec", Arguments.Contain(Set()), None), parser.parse(parser.pStmt, """exec(...);""").get)
    assertEquals(Invoke("exec", Arguments.Contain(Set(RegexLit("su*"))), None), parser.parse(parser.pStmt, """exec(..., r"su*");""").get)
    assertEquals(Invoke("setIntentData", Arguments.Are(List(LitExpr(StringLit("market://details?id=com.great.animalpop")))), None),
      parser.parse(parser.pStmt, """setIntentData("market://details?id=com.great.animalpop");""").get)
  }

  @Test def testParseMethod(): Unit = {
    val parser = new SpecParser()
    assertEquals(MethodSpec(NamedWildcard("_r0"), NamedWildcard("f"), Map(), BodySpec.Just(List(Invoke("exec", Arguments.Contain(Set()), None)))),
      parser.parse(parser.pMethodSpec,
        """_ _f(...) {
          |  exec(...);
          |}""".stripMargin).get)
    assertEquals(MethodSpec(NamedWildcard("ret"), StringIdentifier("onClick"), Map(), BodySpec.Just(List())),
      parser.parse(parser.pMethodSpec,
        """_ret onClick(...) {
          |}""".stripMargin).get)

    assertEquals(MethodSpec(NamedWildcard("ret"), StringIdentifier("onClick"), Map(),
      BodySpec.Or(BodySpec.Just(List(Invoke("exec1", Arguments.Contain(Set()), None))), BodySpec.Just(List(Invoke("exec2", Arguments.Contain(Set()), None))))),
      parser.parse(parser.pMethodSpec,
        """_ret onClick(...) {
          |  {
          |    exec1(...);
          |  } or {
          |    exec2(...);
          |  }
          |}""".stripMargin).get)

    // FIXME: "_ f(...) {}" doesn't work
  }

  @Test def testParseParentClass(): Unit = {
    val parser = new SpecParser()
    assertEquals(None, parser.parse(parser.pParentClass, "").get)
    assertEquals(Some(StringIdentifier("android.content.Context")),
      parser.parse(parser.pParentClass, """(android.content.Context)""").get)
  }

  @Test def testParseConstraintDecl(): Unit = {
    val parser = new SpecParser()
    assertEquals(ConstraintDecl("reachable", List("entrypoint", "f")),
      parser.parse(parser.pConstraintDecl, "reachable(entrypoint, f)").get)
  }

  @Test def testParseClass(): Unit = {
    val parser = new SpecParser()
    assertEquals(ClassSpec(NamedWildcard("H"), None,
      List(MethodSpec(NamedWildcard("_r0"), NamedWildcard("f"), Map(), BodySpec.Just(List(
        Invoke("exec", Arguments.Contain(Set()), None),
        Invoke("setIntentPackage", Arguments.Are(List(LitExpr(StringLit("com.android.vending")))), None)))))),
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
