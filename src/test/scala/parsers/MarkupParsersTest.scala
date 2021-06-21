package dev.vgerasimov.scorg
package parsers

import models.markup.Content._
import models.markup._
import parsers.markup._

import fastparse._

class MarkupParsersTest extends munit.ScalaCheckSuite {

  test("TEXT MARKUP should parse some valid markup string") {
    val toParse = """*hello world*"""
    val parsed = parse(toParse, textMarkup(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(value, bold("", "hello world"))
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }

  test("CONTENTS should parse some valid text string") {
    val toParse = """*hello world*, this _is_ my +long+ formatted ~message~!"""
    val parsed = parse(toParse, contents(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          Contents(
            List(
              bold("", "hello world"),
              text(", this"),
              unde(" ", "is"),
              text(" my"),
              stri(" ", "long"),
              text(" formatted"),
              code(" ", "message"),
              text("!")
            )
          )
        )
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }

  test("CONTENTS should parse nested formatted string") {
    val toParse =
      """/this _is *my* +long+ formatted_ message/!
        |=this is ~nested in verbatim~=
        |~this is =nested in code=~
        |""".stripMargin
    val parsed = parse(toParse, contents(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          Contents(
            TextMarkup(
              "",
              Marker.Italic,
              Contents(
                Text("this"),
                TextMarkup(
                  " ",
                  Marker.Underline,
                  Contents(
                    Text("is"),
                    TextMarkup(" ", Marker.Bold, "my"),
                    TextMarkup(" ", Marker.StrikeThrough, "long"),
                    Text(" formatted")
                  ),
                ),
                Text(" message")
              ),
            ),
            text("!"),
            TextMarkup("\n", Marker.Verbatim, "this is ~nested in verbatim~"),
            TextMarkup("\n", Marker.Code, "this is =nested in code="),
            text("\n")
          )
        )
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }
}
