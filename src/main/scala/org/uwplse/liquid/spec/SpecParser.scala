package org.uwplse.liquid.spec
import org.uwplse.liquid.spec.Expr.{IdExpr, LitExpr}
import org.uwplse.liquid.spec.IdentifierPattern.{NamedWildcard, StringIdentifier}
import org.uwplse.liquid.spec.Literal.{BoolLit, IntLit, StringLit}
import org.uwplse.liquid.spec.PatternDecl.MethodSignature
import org.uwplse.liquid.spec.StatementSpec.Invoke

import scala.util.parsing.combinator._

sealed trait Token
case object UNDERSCORE extends Token
case object DOT extends Token
case object DOUBLEQUOTE extends Token
case object LEFTBRACE extends Token
case object RIGHTBRACE extends Token
case object LEFTCURLYBRACE extends Token
case object RIGHTCURLYBRACE extends Token
case object COMMA extends Token
case object SEMICOLON extends Token

class SpecParser extends RegexParsers {
  var counter: Int = 0
  var names: collection.mutable.Set[String] = collection.mutable.Set()
  def getFreshName: String = {
    while (names.contains("_r%s".format(counter))) {
      counter += 1
    }
    "_r%s".format(counter)
  }

  def word: Parser[String]      = """[a-zA-Z]+""".r    ^^ { _.toString }
  def stringId: Parser[String]      = """[<>a-zA-Z\\.]+""".r    ^^ { _.toString }
  def stringLit: Parser[String] = """(\\.|[^"\\])*""".r    ^^ { _.toString }
  def underscore: Parser[Token] = "_"                  ^^ (_ => UNDERSCORE)
  def leftParen: Parser[Token] = """\(""".r            ^^ (_ => LEFTBRACE)
  def rightParen: Parser[Token] = """\)""".r           ^^ (_ => RIGHTBRACE)
  def leftCurlyBrace: Parser[Token] = """\{""".r       ^^ (_ => LEFTCURLYBRACE)
  def rightCurlyBrace: Parser[Token] = """}""".r       ^^ (_ => RIGHTCURLYBRACE)
  def doubleQuote: Parser[Token] = '"'                 ^^ (_ => DOUBLEQUOTE)
  def semiColon: Parser[Token] = """;""".r             ^^ (_ => SEMICOLON)
  def dot:        Parser[Token] = "."                  ^^ (_ => DOT)
  def comma:        Parser[Token] = """, *""".r        ^^ (_ => COMMA)
  def number: Parser[Int]       = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def anyArgs: Parser[String] = """\.\.\.""".r         ^^ { _.toString }

  def pId: Parser[IdentifierPattern] = {
    val namedWildcard = underscore ~ word ^^ {
      case _ ~ wd => {
        names.add(wd)
        NamedWildcard(wd)
      }
    }
    val wildcard = underscore ^^ (_ => {
      val name = getFreshName
      names.add(name)
      NamedWildcard(name)
    })
    val idString = stringId ^^ (s => StringIdentifier(s))
    namedWildcard | wildcard | idString
  }
  def pDecl: Parser[PatternDecl] = {
    val methodSig = """methodSig""".r ~ word ~ leftParen ~ pId ~ comma ~ pId ~ rightParen ^^ {
      case _ ~ name ~ _ ~ classId ~ _ ~ methodId ~ _ => MethodSignature(name, classId, methodId)
    }
    methodSig
  }
  def pLiteral: Parser[Literal] = {
    val stringLiteral = doubleQuote ~ stringLit ~ doubleQuote ^^ { case _ ~ s ~ _ => StringLit(s) }
    val intLit = number ^^ { i => IntLit(i) }
    val trueLit = """true""".r ^^ { _ => BoolLit(true) }
    val falseLit = """false""".r ^^ { _ => BoolLit(false) }
    stringLiteral | intLit | trueLit | falseLit
  }
  def pExpr: Parser[Expr] = {
    val pIdExpr = pId ^^ (l => IdExpr(l))
    val pLitExpr = pLiteral ^^ (l => LitExpr(l))
    pLitExpr | pIdExpr
  }
  def pStmt: Parser[StatementSpec] =
    word ~ leftParen ~ anyArgs ~ (comma ~ pExpr).* ~ rightParen ~ semiColon ^^ {
      case name ~ _ ~ _ ~ args ~ _ ~ _ => Invoke(name, args.map(_._2))
    }
  def pMethodSpec: Parser[MethodSpec] = {
    pId ~ pId ~ leftParen ~ anyArgs ~ rightParen ~ leftCurlyBrace ~ pStmt.* ~ rightCurlyBrace ^^ {
      case retId ~ name ~ _ ~ _ ~ _ ~ _ ~ stmts ~ _ => MethodSpec(retId, name, stmts)
    }
  }
  def pParentClass : Parser[Option[IdentifierPattern]] = {
    val parentId = leftParen ~ pId ~ rightParen ^^ { case _ ~ name ~ _ => Some(name) }
    val noParent = "" ^^ { _ => None }
    parentId | noParent
  }
  def pAttr: Parser[Attribute] = {
    pId ~ pId ~ semiColon ^^ {
      case typeId ~ varId ~ _ => Attribute(typeId, varId)
    }
  }
  def pClassSpec: Parser[ClassSpec] = {
    """class""".r ~ pId ~ pParentClass ~ leftCurlyBrace ~ pAttr.* ~ pMethodSpec.* ~ rightCurlyBrace ^^ {
      case _ ~ name ~ parent ~ _ ~ attributes ~ methods ~ _ => ClassSpec(name, parent, attributes, methods)
    }
  }
  def pAppSpec: Parser[AppSpec] = {
    pDecl.* ~ pClassSpec.* ^^ {
      case patterns ~ classes => AppSpec(patterns, classes)
    }
  }
}
