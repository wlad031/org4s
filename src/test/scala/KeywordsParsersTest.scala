package dev.vgerasimov.scorg

import models.elements.Keyword._
import models.objects.{ Text, TextMarkup }

class KeywordsParsersTest extends ParserCheckSuite {

  test("TABLE FORMULA should parse table formula keyword") {
    checkParser(
      parser.keyword.tableFormula(_),
      """#+TBLFM: this-is-formula""",
      TableFormula("this-is-formula")
    )
  }

  test("CALL should parse Babel call keyword") {
    checkParser(
      parser.keyword.call(_),
      """#+CALL: this-is-call""",
      Call("this-is-call")
    )
  }

  test("AFFILIATED KEYWORD should parse results keyword with no optional") {
    checkParser(
      parser.keyword.affiliatedKeyword(_),
      """#+RESULTS: this-is-result""",
      Affiliated.Results("this-is-result")
    )
  }

  test("AFFILIATED KEYWORD should parse results keyword with optional") {
    checkParser(
      parser.keyword.affiliatedKeyword(_),
      """#+RESULTS[this-is-optional]: this-is-result""",
      Affiliated.Results("this-is-result", Some("this-is-optional"))
    )
  }

  test("KEYWORD should parse simple title keyword") {
    checkParser(
      parser.keyword.keyword(_),
      """#+TITLE: this is title""",
      GenericKeyword("TITLE", List(Text("this is title")))
    )
  }

  test("KEYWORD should parse title keyword with markup") {
    checkParser(
      parser.keyword.keyword(_),
      """#+TITLE: this _is_ title""",
      GenericKeyword("TITLE", List(Text("this"), TextMarkup.underline(" ", "is"), Text(" title")))
    )
  }
}
