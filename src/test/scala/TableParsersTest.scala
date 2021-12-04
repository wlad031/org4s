package dev.vgerasimov.org4s

import models.greater_elements.Table
import ops.table._

import fastparse._

class TableParsersTest extends ParserCheckSuite {

  test("TABLE should parse simple valid table") {
    val toParse =
      """|| Name  | Phone | Age |
         ||-------+-------+-----|
         || Peter |  1234 |  17 |
         || Anna  |  4321 |  25 |
         |""".stripMargin
    val parsed = parse(toParse, parser.table.table(_))
    parsed match {
      case Parsed.Success(value, _) =>
        val expected: Table =
          ($("Name") | $("Phone") | $("Age")) +
          sep +
          ($("Peter") | $("1234") | $("17")) +
          ($("Anna") | $("4321") | $("25"))
        assertEquals(value, expected)
      case r: Parsed.Failure => fail(s"$toParse not parsed: $r")
    }
  }
}
