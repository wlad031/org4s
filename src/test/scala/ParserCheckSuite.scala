package dev.vgerasimov.org4s

import dev.vgerasimov.slowparse.{ P, POut }

/** Generic suite for testing Org parsers. */
trait ParserCheckSuite extends munit.ScalaCheckSuite {

  protected lazy val ctx: OrgContext = OrgContext.defaultCtx
  protected lazy val parser: OrgParser = new OrgParser(ctx)

  extension [T](r: POut[T])
    def isSuccess: Boolean = r match
      case _: POut.Success[_] => true
      case _: POut.Failure => false

  export dev.vgerasimov.slowparse.POut

  def parse[T](toParse: String, parser: P[T]): POut[T] = parser(toParse)

  def checkParser[T](parser: P[T], toParse: String, expected: => T): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) =>
        assertEquals(value, expected)
      case _: POut.Failure => fail(s"$toParse not parsed")
    }

  def checkParserFailed[T](parser: P[T], toParse: String): Unit =
    parse(toParse, parser) match {
      case POut.Success(value, _, _, _) => fail(s"$toParse parsed to $value")
      case _                        =>
    }
}
