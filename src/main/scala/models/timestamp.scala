package dev.vgerasimov.scorg
package models

/** Contains models representing timestamp elements. */
object timestamp {

  case class Hour(value: Int) {
    require(Hour.isValid(value), s"Invalid hour value: $value")

    override def toString: String = f"$value%02d"
  }

  object Hour {

    /** Checks that provided value is valid hour. */
    def isValid(hour: Int): Boolean = 0 <= hour && hour <= 23
  }

  case class Minute(value: Int) {
    require(Minute.isValid(value), s"Invalid minute value: $value")

    override def toString: String = f"$value%02d"
  }

  object Minute {

    /** Checks that provided value is valid minute. */
    def isValid(minute: Int): Boolean = 0 <= minute && minute <= 59
  }

  case class Time(hour: Hour, minute: Minute) {
    override def toString: String = s"$hour:$minute"
  }

  object Time {
    def of(hour: Int, minute: Int): Time = Time(Hour(hour), Minute(minute))
  }

  case class Year(value: Int) {
    require(Year.isValid(value), s"Invalid year value: $value")

    /** Indicates whether this year is leap or not. */
    def isLeap: Boolean =
      if (value % 4 != 0) false
      else if (value % 100 != 0) true
      else if (value % 400 != 0) false
      else true

    override def toString: String = f"$value%04d"
  }

  object Year {

    /** Checks that provided value is valid year. */
    def isValid(year: Int): Boolean = 0 <= year
  }

  case class Month(value: Int) {
    require(Month.isValid(value), s"Invalid month value: $value")

    /** Returns the number of days in this month. */
    def getNumberOfDays(isLeapYear: Boolean = false): Int =
      if (value == 2 && isLeapYear) Month.days(value - 1) + 1
      else Month.days(value - 1)

    override def toString: String = f"$value%02d"
  }

  object Month {

    /** Checks that provided value is valid month. */
    def isValid(month: Int): Boolean = 1 <= month && month <= 12

    /** Numbers of days in all 12 months for non-leap year. */
    private val days: Array[Int] = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }

  case class Day(value: Int) {
    require(Day.isValid(value), s"Invalid day value: $value")

    override def toString: String = f"$value%02d"
  }

  object Day {

    /** Checks that provided value is valid day. */
    def isValid(day: Int): Boolean = 1 <= day && day <= 31
  }

  sealed trait DayName {
    override def toString: String =
      this match {
        case DayName.Monday    => "Mon"
        case DayName.Tuesday   => "Tue"
        case DayName.Wednesday => "Wed"
        case DayName.Thursday  => "Thu"
        case DayName.Friday    => "Fri"
        case DayName.Saturday  => "Sat"
        case DayName.Sunday    => "Sun"
      }
  }

  object DayName {
    case object Monday extends DayName
    case object Tuesday extends DayName
    case object Wednesday extends DayName
    case object Thursday extends DayName
    case object Friday extends DayName
    case object Saturday extends DayName
    case object Sunday extends DayName

    def fromString(dayName: String): Option[DayName] =
      dayName match {
        case x if Set("Mon") contains x => Some(Monday)
        case x if Set("Tue") contains x => Some(Tuesday)
        case x if Set("Wed") contains x => Some(Wednesday)
        case x if Set("Thu") contains x => Some(Thursday)
        case x if Set("Fri") contains x => Some(Friday)
        case x if Set("Sat") contains x => Some(Saturday)
        case x if Set("Sun") contains x => Some(Sunday)
        case _                          => None
      }
  }

  case class Date(year: Year, month: Month, day: Day, dayName: Option[DayName] = None) {
    require(Date.isValid(year, month, day), s"Invalid date: year=$year, month=$month, day=$day")

    override def toString: String =
      dayName match {
        case Some(dn) => s"$year-$month-$day $dn"
        case None     => s"$year-$month-$day"
      }
  }

  object Date {

    def of(year: Int, month: Int, day: Int, dayName: Option[String] = None): Date =
      Date(Year(year), Month(month), Day(day), dayName.flatMap(DayName.fromString))

    /** Checks that provided values represent a valid date. */
    def isValid(year: Year, month: Month, day: Day): Boolean =
      day.value <= month.getNumberOfDays(year.isLeap)
  }

  sealed trait RepeaterOrDelay

  object RepeaterOrDelay {
    sealed trait Repeater extends RepeaterOrDelay
    sealed trait Delay extends RepeaterOrDelay
    case class And(repeater: Repeater, delay: Delay) extends RepeaterOrDelay

    case class CumulateRepeater(value: Value, unit: Unit) extends Repeater
    case class CatchUpRepeater(value: Value, unit: Unit) extends Repeater
    case class RestartRepeater(value: Value, unit: Unit) extends Repeater
    case class AllTypeDelay(value: Value, unit: Unit) extends Delay
    case class FirstTypeDelay(value: Value, unit: Unit) extends Delay

    case class Value(value: Int)

    sealed trait Unit
    object Unit {
      case object Hour extends Unit
      case object Day extends Unit
      case object Week extends Unit
      case object Month extends Unit
      case object Year extends Unit

      def fromString(unit: String): Option[Unit] =
        unit match {
          case x if Set("h").contains(x) => Some(Hour)
          case x if Set("d").contains(x) => Some(Day)
          case x if Set("w").contains(x) => Some(Week)
          case x if Set("m").contains(x) => Some(Month)
          case x if Set("y").contains(x) => Some(Year)
          case _                         => None
        }
    }
  }

  sealed trait Timestamp
  object Timestamp {
    sealed trait Active extends Timestamp
    sealed trait Inactive extends Timestamp
    sealed trait Range extends Timestamp
  }

  case class Diary(value: String) extends Timestamp

  case class ActiveTimestamp(
    date: Date,
    time: Time,
    repeaterOrDelay: RepeaterOrDelay
  ) extends Timestamp
      with Timestamp.Active

  case class InactiveTimestamp(
    date: Date,
    time: Time,
    repeaterOrDelay: RepeaterOrDelay
  ) extends Timestamp
      with Timestamp.Inactive

  case class ActiveTimestampRange(
    from: ActiveTimestamp,
    to: ActiveTimestamp
  ) extends Timestamp
      with Timestamp.Active
      with Timestamp.Range

  object ActiveTimestampRange {
    def apply(from: ActiveTimestamp, toTime: Time): ActiveTimestampRange =
      ActiveTimestampRange(from, from.copy(time = toTime))
  }

  case class InactiveTimestampRange(
    from: InactiveTimestamp,
    to: InactiveTimestamp
  ) extends Timestamp
      with Timestamp.Inactive
      with Timestamp.Range

  object InactiveTimestampRange {
    def apply(from: InactiveTimestamp, toTime: Time): InactiveTimestampRange =
      InactiveTimestampRange(from, from.copy(time = toTime))
  }

  case class Duration(hours: Int, minutes: Int)

  sealed trait Clock
  object Clock {
    case class Simple(timestamp: Timestamp.Inactive) extends Clock
    case class WithDuration(timestamp: Timestamp.Inactive, duration: Duration) extends Clock
  }
}
