package dev.vgerasimov.scorg

import context.OrgContext

import fastparse._

trait ParserCheckSuite extends munit.ScalaCheckSuite {

  protected lazy val ctx: OrgContext = OrgContext.defaultCtx
  protected lazy val parser: OrgParser = new OrgParser()(ctx)

  def checkParser[T](parser: P[_] => P[T], toParse: String, expected: => T): Unit =
    parse(toParse, parser) match {
      case Parsed.Success(value, _) =>
        assertEquals(value, expected)
      case _: Parsed.Failure => fail(s"$toParse not parsed")
    }

  def checkParserFailed[T](parser: P[_] => P[T], toParse: String): Unit =
    parse(toParse, parser) match {
      case Parsed.Success(value, _) => fail(s"$toParse parsed to $value")
      case _                        =>
    }
}
