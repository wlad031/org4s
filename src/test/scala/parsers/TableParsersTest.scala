package dev.vgerasimov.scorg
package parsers

import models.table._
import parsers.table._

import fastparse._

class TableParsersTest extends munit.ScalaCheckSuite {

  test("TABLE should parse simple valid table") {
    val toParse =
      """|| Name  | Phone | Age |
         ||-------+-------+-----|
         || Peter |  1234 |  17 |
         || Anna  |  4321 |  25 |
         |""".stripMargin
    val parsed = parse(toParse, table(_))
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
