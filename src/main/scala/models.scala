package dev.vgerasimov.scorg

import models.elements.Planning

object models {

  case class Document(elements: List[Element], sections: List[Section])

  case class Headline(
    stars: Headline.Stars,
    keyword: Option[Headline.Keyword] = None,
    priority: Option[Headline.Priority] = None,
    title: Option[Headline.Title] = None,
    tags: Option[Headline.Tags] = None,
    isComment: Boolean = false
  ) {
    // TODO: refactor
    override def toString: String = {
      val sb = new StringBuilder(stars.toString)
      def appendSome[A](o: Option[A]): Unit =
        o match {
          case Some(value) => sb.append(" ").append(value)
          case None        =>
        }
      appendSome(keyword)
      appendSome(priority)
      if (isComment) {
        sb.append(" COMMENT")
      }
      appendSome(title)
      appendSome(tags)
      sb.toString()
    }
  }

  object Headline {
    sealed trait Keyword
    object Keyword {
      case class Todo(value: String) extends Keyword {
        override def toString: String = value
      }

      case class Done(value: String) extends Keyword {
        override def toString: String = value
      }
    }

    case class Priority(value: Char) {
      override def toString: String = s"[#$value]"
    }

    case class Tags(tags: List[String]) {
      require(tags.nonEmpty, "List of tags cannot be empty")
      override def toString: String = tags.mkString(start = ":", sep = ":", end = ":")
    }

    case class Title(contents: List[Title.Content]) {
      override def toString: String = contents.mkString
    }

    object Title {
      def apply(s: String): Title = new Title(List(objects.Text(s)))
      def apply(contents: Content*): Title = new Title(contents.toList)

      def withTextFold(contents: Seq[Content]): Title =
        Title(
          contents
            .foldLeft[List[Content]](Nil)((ls, item) =>
              ls match {
                case Nil => List(item)
                case head :: tail =>
                  (item, head) match {
                    case (t1: objects.Text, t2: objects.Text) => (t2 ++ t1) :: tail
                    case (t1, t2)                             => t1 :: t2 :: tail
                  }
              }
            )
            .reverse
        )

      sealed trait Content
    }

    case class Stars(n: Int) {
      require(n > 0, "The number of stars cannot be less than 1")
      override def toString: String = "*".repeat(n)
    }
  }

  case class Section(
    headline: Headline,
    elements: List[Element] = Nil,
    childSections: List[Section] = Nil,
    planning: Option[Planning] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  )

  case class PropertyDrawer(nodes: List[elements.NodeProperty])

  /** Base type for all Org greater elements. */
  sealed trait GreaterElement extends Element

  /** Base type for all Org elements. */
  sealed trait Element

  /** Base type for all [[https://orgmode.org/worg/dev/org-syntax.html#Objects Org objects]]. */
  sealed trait OrgObject

  /** Contains implementations for [[GreaterElement]]s. */
  object greater_elements {

    case class Table(rows: List[elements.TableRow], formulas: List[elements.Keyword.TableFormula])
        extends GreaterElement {
      def + (that: elements.TableRow): Table = Table(rows ++ List(that), formulas)
      def + (formula: elements.Keyword.TableFormula): Table = Table(rows, formulas ++ List(formula))
    }

    sealed trait PlainList extends GreaterElement
    object PlainList {
      case class Item(
        indentation: Int,
        bullet: Bullet,
        checkbox: Option[Checkbox],
        counterSet: Option[Counter],
        tag: Option[String],
        contents: List[Content],
        elements: List[Element]
      )

      case class Counter(value: String)

      sealed trait Bullet
      object Bullet {
        case class Character(value: Char) extends Bullet
        case class Ordered(value: Counter, character: Char) extends Bullet
      }

      sealed trait Checkbox
      object Checkbox {
        case object Empty extends Checkbox
        case object Checked extends Checkbox
        case object Unchecked extends Checkbox
      }

      sealed trait Content

      case class Simple(items: List[Item]) extends PlainList
      case class Ordered(items: List[Item]) extends PlainList
    }
  }

  /** Contains implementations for [[Element]]s. */
  object elements {

    case class EmptyLines(length: Int) extends Element

    case class FixedWidthArea(lines: List[FixedWidthArea.Line]) extends Element
    object FixedWidthArea {
      case class Line(value: String)
    }

    case class HorizontalRuler(length: Int) extends Element {
      require(length >= 5, "Horizontal ruler should be 5 or more characters long")
    }

    sealed trait Keyword extends Element
    object Keyword {
      case class TableFormula(formulas: String) extends Keyword
      case class Call(value: String) extends Keyword

      sealed trait Affiliated extends Keyword
      object Affiliated {
        case class AttrBackend(backend: String, value: String) extends Affiliated
        case class Caption(value: List[Content], optional: Option[List[Content]] = None)
            extends Affiliated
        case class Header(value: String) extends Affiliated
        case class Name(value: String) extends Affiliated
        case class Plot(value: String) extends Affiliated
        case class Results(value: String, optional: Option[String] = None) extends Affiliated
      }

      case class GenericKeyword(key: String, value: List[Content]) extends Keyword

      sealed trait Content
    }

    case class Paragraph(objects: List[OrgObject]) extends Element {
      def ++ (that: Paragraph): Paragraph = Paragraph(this.objects ++ that.objects)
    }

    case class Comments(lines: List[Comments.Line]) extends Element
    object Comments {
      case class Line(value: String)
    }

    sealed trait TableRow {
      import elements.Keyword.TableFormula
      import greater_elements.Table

      def + (that: TableRow): Table = Table(List(this, that), Nil)
      def + (formula: TableFormula): Table = Table(List(this), List(formula))

      def asTable: Table = Table(List(this), Nil)
    }

    object TableRow {
      import objects.TableCell

      case object TableSep extends TableRow
      case class TableRowCells(cells: List[TableCell]) extends TableRow {
        def | (cell: TableCell): TableRowCells = TableRowCells(cells ++ List(cell))
      }
    }

    case class Planning(info: List[Planning.Info])
    object Planning {
      sealed trait Info
      object Info {
        import objects.Timestamp

        case class Deadline(timestamp: Timestamp) extends Info
        case class Scheduled(timestamp: Timestamp) extends Info
        case class Closed(timestamp: Timestamp) extends Info
      }
    }

    case class NodeProperty(name: String, value: Option[String] = None) extends Element
  }

  /** Contains implementations for [[OrgObject]]s. */
  object objects {
    case object LineBreak extends OrgObject

    case class Text(value: String)
        extends OrgObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content
        with elements.Keyword.Content {
      override def toString: String = s"T($value)"

      def ++ (that: Text): Text = Text(this.value + that.value)
    }

    sealed trait Marker {
      val isNestable: Boolean = this match {
        case Marker.Code     => false
        case Marker.Verbatim => false
        case _               => true
      }

      override def toString: String =
        this match {
          case Marker.Bold          => "*"
          case Marker.Verbatim      => "="
          case Marker.Italic        => "/"
          case Marker.StrikeThrough => "+"
          case Marker.Underline     => "_"
          case Marker.Code          => "~"
        }
    }

    object Marker {
      case object Bold extends Marker
      case object Verbatim extends Marker
      case object Italic extends Marker
      case object StrikeThrough extends Marker
      case object Underline extends Marker
      case object Code extends Marker

      def fromString(s: String): Option[Marker] =
        s match {
          case "*" => Some(Bold)
          case "=" => Some(Verbatim)
          case "/" => Some(Italic)
          case "+" => Some(StrikeThrough)
          case "_" => Some(Underline)
          case "~" => Some(Code)
          case _   => None
        }
    }

    case class TextMarkup(pre: String, marker: Marker, contents: List[TextMarkup.Content])
        extends OrgObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content
        with elements.Keyword.Content {
      override def toString: String = s"MT($marker${contents}$marker)"
    }

    object TextMarkup {
      sealed trait Content

      def apply(marker: Marker, text: String): TextMarkup =
        new TextMarkup("", marker, List(Text(text)))

      def apply(pre: String, marker: Marker, text: String): TextMarkup =
        new TextMarkup(pre, marker, List(Text(text)))

      def bold(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Bold, List(Text(value)))

      def verbatim(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Verbatim, List(Text(value)))

      def italic(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Italic, List(Text(value)))

      def strikeThrough(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.StrikeThrough, List(Text(value)))

      def underline(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Underline, List(Text(value)))

      def code(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Code, List(Text(value)))
    }

    case class Contents(contents: List[OrgObject]) {
      override def toString: String = contents.mkString
    }

    object Contents {
      def apply(contents: OrgObject*): Contents = new Contents(contents.toList)
    }

    sealed trait Link
        extends OrgObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content
        with elements.Keyword.Content

    object Link {
      case class RadioLink(target: String) extends Link
      case class AngleLink(protocol: Protocol, path: String) extends Link
      case class PlainLink(protocol: Protocol, path: String) extends Link

      sealed trait RegularLink extends Link

      object RegularLink {
        case class FilenameLink(value: String, description: Contents) extends RegularLink
        case class ProtocolLink(
          protocol: Protocol,
          path: String,
          description: Option[Contents] = None
        ) extends RegularLink
        case class IdLink(id: String) extends RegularLink
        case class CustomIdLink(customId: String) extends RegularLink
        case class CodeRefLink(codeRef: String) extends RegularLink
        case class FuzzyLink(fuzzy: String) extends RegularLink
      }

      case class Protocol(value: String)
    }

    sealed trait Timestamp
        extends OrgObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content
        with elements.Keyword.Content

    object Timestamp {
      sealed trait Active extends Timestamp
      sealed trait Inactive extends Timestamp
      sealed trait Range extends Timestamp

      case class Diary(value: String) extends Timestamp

      case class ActiveTimestamp(
        date: Date,
        time: Option[Time],
        repeaterOrDelay: Option[RepeaterOrDelay] = None
      ) extends Timestamp
          with Timestamp.Active

      case class InactiveTimestamp(
        date: Date,
        time: Option[Time],
        repeaterOrDelay: Option[RepeaterOrDelay] = None
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
          ActiveTimestampRange(from, from.copy(time = Some(toTime)))
      }

      case class InactiveTimestampRange(
        from: InactiveTimestamp,
        to: InactiveTimestamp
      ) extends Timestamp
          with Timestamp.Inactive
          with Timestamp.Range

      object InactiveTimestampRange {
        def apply(from: InactiveTimestamp, toTime: Time): InactiveTimestampRange =
          InactiveTimestampRange(from, from.copy(time = Some(toTime)))
      }

      case class Time(hour: Time.Hour, minute: Time.Minute) {
        override def toString: String = s"$hour:$minute"
      }

      object Time {
        def of(hour: Int, minute: Int): Time = Time(Hour(hour), Minute(minute))

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
      }

      case class Date(
        year: Date.Year,
        month: Date.Month,
        day: Date.Day,
        dayName: Option[Date.DayName] = None
      ) {
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
    }

    case class Duration(hours: Int, minutes: Int)

    sealed trait Clock
    object Clock {
      case class Simple(timestamp: Timestamp.Inactive) extends Clock
      case class WithDuration(timestamp: Timestamp.Inactive, duration: Duration) extends Clock
    }

    sealed trait StatCookie extends OrgObject with Headline.Title.Content
    object StatCookie {

      case object EmptyPercent extends StatCookie
      case object EmptyFractional extends StatCookie

      case class Percent(value: Int) extends StatCookie {
        require(Percent.isValid(value), s"Invalid percent stat cookie value: $value")
      }

      object Percent {
        def isValid(percent: Int): Boolean = 0 <= percent && percent <= 100
      }

      case class Fractional(amount: Int, total: Int) extends StatCookie {
        require(
          Fractional.isValid(amount, total),
          s"Invalid values of fractional stat cookie: amount=$amount, total=$total"
        )
      }

      object Fractional {
        def isValid(amount: Int, total: Int): Boolean = 0 <= amount && 0 <= total && amount <= total
      }
    }

    case class TableCell(value: String) {
      import elements.TableRow.TableRowCells

      def | (that: TableCell): TableRowCells = TableRowCells(List(this, that))
    }

    case class Target(target: String)
    case class RadioTarget(contents: String)
  }

}
