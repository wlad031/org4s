package dev.vgerasimov.scorg
package parsers

import models.timestamp._

import fastparse.NoWhitespace._
import fastparse._

/** Contains parsers for [[models.timestamp]] objects. */
object timestamp {

  def minute[_ : P]: P[Minute] =
    P(digit.rep(exactly = 2).!)
      .map(_.toInt)
      .filter(Minute.isValid)
      .map(Minute.apply)

  def hour[_ : P]: P[Hour] =
    P(digit.rep(min = 1, max = 2).!)
      .map(_.toInt)
      .filter(Hour.isValid)
      .map(Hour.apply)

  def time[_ : P]: P[Time] =
    P(hour ~ ":" ~ minute)
      .map({ case (hour, minute) => Time(hour, minute) })

  def year[_ : P]: P[Year] =
    P(digit.rep(exactly = 4).!)
      .map(_.toInt)
      .filter(Year.isValid)
      .map(Year.apply)

  def month[_ : P]: P[Month] =
    P(digit.rep(exactly = 2).!)
      .map(_.toInt)
      .filter(Month.isValid)
      .map(Month.apply)

  def day[_ : P]: P[Day] =
    P(digit.rep(exactly = 2).!)
      .map(_.toInt)
      .filter(Day.isValid)
      .map(Day.apply)

  def dayName[_ : P]: P[DayName] =
    P(anyNotIn("+-]> \t\n\r0123456789").!)
      .map(DayName.fromString)
      .filter(_.isDefined)
      .map(_.get)

  def date[_ : P]: P[Date] =
    P(year ~ "-" ~ month ~ "-" ~ day ~ (" " ~ dayName).?)
      .filter({ case (year, month, day, _) => Date.isValid(year, month, day) })
      .map({ case (year, month, day, dayName) => Date(year, month, day, dayName) })

  def diary[_ : P]: P[Diary] =
    P("<%%(" ~ anyNotIn("\n>") ~ ")>")
      .map(Diary.apply)

  def repeaterMark[_ : P]: P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
    (P("++").map(_ => RepeaterOrDelay.CatchUpRepeater)
    | P("+").map(_ => RepeaterOrDelay.CumulateRepeater)
    | P(".+").map(_ => RepeaterOrDelay.RestartRepeater)
    | P("--").map(_ => RepeaterOrDelay.FirstTypeDelay)
    | P("-").map(_ => RepeaterOrDelay.AllTypeDelay))

  def repeaterValue[_ : P]: P[RepeaterOrDelay.Value] =
    P(digit.rep(1).!)
      .map(_.toInt)
      .map(RepeaterOrDelay.Value.apply)

  def repeaterUnit[_ : P]: P[RepeaterOrDelay.Unit] =
    P(CharIn("hdwmy").!)
      .map(RepeaterOrDelay.Unit.fromString)
      .filter(_.isDefined)
      .map(_.get)

  def repeaterOrDelay[_ : P]: P[RepeaterOrDelay] =
    P(repeaterMark ~ repeaterValue ~ repeaterUnit)
      .map({ case (factory, value, unit) => factory(value, unit) })

  def timestamp[_ : P]: P[Timestamp] =
    P(
      diary
      | activeTimestampRange
      | activeTimestamp
      | inactiveTimestampRange
      | inactiveTimestamp
    )

  def activeTimestamp[_ : P]: P[ActiveTimestamp] =
    P("<" ~ date ~ s ~ time ~ s ~ repeaterOrDelay ~ ">")
      .map({ case (d, t, r) => ActiveTimestamp(d, t, r) })

  def inactiveTimestamp[_ : P]: P[InactiveTimestamp] =
    P("[" ~ date ~ s ~ time ~ s ~ repeaterOrDelay ~ "]")
      .map({ case (d, t, r) => InactiveTimestamp(d, t, r) })

  def activeTimestampRange[_ : P]: P[ActiveTimestampRange] =
    P(activeTimestamp ~ "--" ~ activeTimestamp).map({
      case (from, to) => ActiveTimestampRange(from, to)
    }) | P("<" ~ date ~ s ~ time ~ time ~ repeaterOrDelay ~ ">")
      .map({ case (d, t1, t2, r) => ActiveTimestampRange(ActiveTimestamp(d, t1, r), t2) })

  def inactiveTimestampRange[_ : P]: P[InactiveTimestampRange] =
    P(inactiveTimestamp ~ "--" ~ inactiveTimestamp)
      .map({ case (from, to) => InactiveTimestampRange(from, to) }) |
    P("[" ~ date ~ s ~ time ~ time ~ repeaterOrDelay ~ "]")
      .map({ case (d, t1, t2, r) => InactiveTimestampRange(InactiveTimestamp(d, t1, r), t2) })

  def duration[_ : P]: P[Duration] =
    P("=>" ~ s ~ digit.rep(1).! ~ ":" ~ digit.rep(exactly = 2).!)
      .map({ case (h, m) => (h.toInt, m.toInt) })
      .map({ case (h, m) => Duration(h, m) })

  def clock[_ : P]: P[Clock] =
    P(
      "CLOCK:" ~ s.? ~ (
        inactiveTimestamp.map(Clock.Simple.apply)
        | (inactiveTimestampRange ~ s ~ duration).map({ case (tr, d) => Clock.WithDuration(tr, d) })
      )
    )
}
