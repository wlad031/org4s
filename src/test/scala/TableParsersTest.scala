package dev.vgerasimov.org4s

import models.greater_elements.Table
import ops.table._

class TableParsersTest extends ParserCheckSuite {

  test("TABLE ROW CELLS should parse single table cell as a table row") {
    val toParse =
      """|| Name |
         |""".stripMargin
    val parsed = parse(toParse, parser.table.tableRowCells(_))
    parsed match {
      case POut.Success(value, _, _, _) =>
        val expected = ($("Name")).asRow
        assertEquals(value, expected)
      case r: POut.Failure => fail(s"$toParse not parsed: $r")
    }
  }

  test("TABLE ROW CELLS should parse 3 table cells as a table row") {
    val toParse =
      """/| Name  | Phone | Age |
         /""".stripMargin('/')
    val parsed = parse(toParse, parser.table.tableRowCells(_))
    parsed match {
      case POut.Success(value, _, _, _) =>
        val expected =
          ($("Name") | $("Phone") | $("Age"))
        assertEquals(value, expected)
      case r: POut.Failure => fail(s"$toParse not parsed: $r")
    }
  }

  test("TABLE should parse 3:3 valid table") {
    val toParse =
      """|| Name  | Phone | Age |
         || Peter |  1234 |  17 |
         || Anna  |  4321 |  25 |
         |""".stripMargin
    val parsed = parse(toParse, parser.table.table(_))
    parsed match {
      case POut.Success(value, _, _, _) =>
        val expected: Table =
          ($("Name") | $("Phone") | $("Age")) +
          ($("Peter") | $("1234") | $("17")) +
          ($("Anna") | $("4321") | $("25"))
        assertEquals(value, expected)
      case r: POut.Failure => fail(s"$toParse not parsed: $r")
    }
  }
}
