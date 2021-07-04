package dev.vgerasimov.scorg

import models.objects.Link._
import models.objects.TextMarkup._
import models.objects._

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

  test("ANY OBJECTS should parse some valid text string") {
    val toParse = """*hello world*, this _is_ my +long+ formatted ~message~!"""
    val parsed = parse(toParse, parser.anyObjects(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          List(
            bold("", "hello world"),
            Text(", this"),
            underline(" ", "is"),
            Text(" my"),
            strikeThrough(" ", "long"),
            Text(" formatted"),
            code(" ", "message"),
            Text("!")
          )
        )
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }

  test("ANY OBJECTS should parse nested formatted string") {
    val toParse =
      """/this _is *my* +long+ formatted_ message/!
        |=this is ~nested in verbatim~=
        |~this is =nested in code=~
        |""".stripMargin
    val parsed = parse(toParse, parser.anyObjects(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          List(
            TextMarkup(
              "",
              Marker.Italic,
              List(
                Text("this"),
                TextMarkup(
                  " ",
                  Marker.Underline,
                  List(
                    Text("is"),
                    TextMarkup(" ", Marker.Bold, "my"),
                    TextMarkup(" ", Marker.StrikeThrough, "long"),
                    Text(" formatted")
                  )
                ),
                Text(" message")
              )
            ),
            Text("!"),
            TextMarkup("\n", Marker.Verbatim, "this is ~nested in verbatim~"),
            TextMarkup("\n", Marker.Code, "this is =nested in code="),
            Text("\n")
          )
        )
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }
}
