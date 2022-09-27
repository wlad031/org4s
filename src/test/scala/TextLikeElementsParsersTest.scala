package dev.vgerasimov.org4s

import models.objects.TextMarkup._

class TextLikeElementsParsersTest extends ParserCheckSuite {

  test("TEXT MARKUP should parse some valid markup string") {
    val toParse = """*hello world*"""
    val parsed = parse(toParse, parser.markup.textMarkup(_))
    parsed match {
      case POut.Success(value, _, _, _) =>
        assertEquals(value, bold("", "hello world"))
      case _: POut.Failure => fail(s"$toParse is not parsed")
    }
  }
}
