package dev.vgerasimov.scorg

import models.objects.TextMarkup._

import fastparse._

class TextLikeElementsParsersTest extends ParserCheckSuite {

  test("TEXT MARKUP should parse some valid markup string") {
    val toParse = """*hello world*"""
    val parsed = parse(toParse, parser.markup.textMarkup(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(value, bold("", "hello world"))
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }
}
