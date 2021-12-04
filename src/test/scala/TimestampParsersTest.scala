package dev.vgerasimov.org4s

import models.objects.Timestamp.Date._
import models.objects.Timestamp.Time._
import models.objects.Timestamp._

import fastparse._
import org.scalacheck.Gen
import org.scalacheck.Prop._

class TimestampParsersTest extends ParserCheckSuite {

  private val genDate = for {
    year  <- Gen.chooseNum(0, 3000).map(Year.apply)
    month <- Gen.chooseNum(1, 12).map(Month.apply)
    day   <- Gen.chooseNum(1, month.getNumberOfDays(year.isLeap)).map(Day.apply)
    dayName <- Gen.option(
      Gen
        .oneOf(Seq("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
        .map(DayName.fromString)
        .map(_.get)
    )
  } yield Date(year, month, day, dayName)

  private val genTime = for {
    hour   <- Gen.chooseNum(0, 23).map(Hour.apply)
    minute <- Gen.chooseNum(0, 59).map(Minute.apply)
  } yield Time(hour, minute)

  test("DATE should parse any valid date string") {
    forAll(genDate) { (d: Date) =>
      {
        val toParse = d.toString
        val parsed = parse(toParse, parser.timestamp.date(_))
        parsed match {
          case Parsed.Success(value, _) => assertEquals(value, d)
          case _: Parsed.Failure        => fail(s"$toParse is not parsed")
        }
      }
    }
  }

  test("TIME should parse any valid time string") {
    forAll(genTime) { (t: Time) =>
      {
        val toParse = t.toString
        val parsed = parse(toParse, parser.timestamp.time(_))
        parsed match {
          case Parsed.Success(value, _) => assertEquals(value, t)
          case _: Parsed.Failure        => fail(s"$toParse is not parsed")
        }
      }
    }
  }

  test("TIMESTAMP should parse valid active timestamp range string") {
    val toParse = "<2020-01-29 Thu 12:34 +5d>--<2021-03-12 23:59 --31h>"
    val parsed = parse(toParse, parser.timestamp.timestamp(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          ActiveTimestampRange(
            ActiveTimestamp(
              Date.of(2020, 1, 29, Some("Thu")),
              Some(Time.of(12, 34)),
              Some(RepeaterOrDelay.CumulateRepeater(RepeaterOrDelay.Value(5), RepeaterOrDelay.Unit.Day))
            ),
            ActiveTimestamp(
              Date.of(2021, 3, 12, None),
              Some(Time.of(23, 59)),
              Some(RepeaterOrDelay.FirstTypeDelay(RepeaterOrDelay.Value(31), RepeaterOrDelay.Unit.Hour))
            )
          )
        )
      case _: Parsed.Failure => fail(s"$toParse is not parsed")
    }
  }
}
