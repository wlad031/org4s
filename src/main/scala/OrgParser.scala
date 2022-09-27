package dev.vgerasimov.org4s

import dev.vgerasimov.org4s.models.*
import dev.vgerasimov.org4s.models.elements.{ EmptyLines, Keyword, Paragraph }
import dev.vgerasimov.org4s.models.greater_elements.PlainList
import dev.vgerasimov.org4s.models.objects.*
import dev.vgerasimov.org4s.ops.*

import dev.vgerasimov.slowparse.*
import dev.vgerasimov.slowparse.Parsers.{ *, given }

class OrgParser(ctx: OrgContext = OrgContext.defaultCtx) {

  private[org4s] object timestamp {
    import models.objects.Timestamp.Date._
    import models.objects.Timestamp.Time._
    import models.objects.Timestamp._

    def minute: P[Minute] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Minute.isValid)
        .map(Minute.apply)

    def hour: P[Hour] =
      digit
        .rep(min = 1, max = 2)
        .!
        .map(_.toInt)
        .filter(Hour.isValid)
        .map(Hour.apply)

    def time: P[Time] =
      (hour ~ P(":") ~ minute).map { case (hour, minute) => Time(hour, minute) }

    def year: P[Year] =
      digit
        .rep(min = 4, max = 4)
        .!
        .map(_.toInt)
        .filter(Year.isValid)
        .map(Year.apply)

    def month: P[Month] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Month.isValid)
        .map(Month.apply)

    def day: P[Day] =
      digit
        .rep(min = 2, max = 2)
        .!
        .map(_.toInt)
        .filter(Day.isValid)
        .map(Day.apply)

    def dayName: P[DayName] =
      charsUntilIn("+-]> \t\n\r0123456789").!.map(DayName.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def date: P[Date] =
      (year ~ P("-") ~ month ~ P("-") ~ day ~ (P(" ") ~ dayName).?).filter { case (year, month, day, _) =>
        Date.isValid(year, month, day)
      }.map { case (year, month, day, dayName) => Date(year, month, day, dayName) }

    def diary: P[Diary] = (P("<%%(") ~ charsUntilIn("\n>") ~ P(")>")).map(Diary.apply)

    def repeaterMark: P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
      (
        P("++").map(_ => RepeaterOrDelay.CatchUpRepeater.apply)
        | P("+").map(_ => RepeaterOrDelay.CumulateRepeater.apply)
        | P(".+").map(_ => RepeaterOrDelay.RestartRepeater.apply)
        | P("--").map(_ => RepeaterOrDelay.FirstTypeDelay.apply)
        | P("-").map(_ => RepeaterOrDelay.AllTypeDelay.apply)
      )

    def repeaterValue: P[RepeaterOrDelay.Value] =
      d
        .rep(1)
        .!
        .map(_.toInt)
        .map(RepeaterOrDelay.Value.apply)

    def repeaterUnit: P[RepeaterOrDelay.Unit] =
      anyFrom("hdwmy").!.map(RepeaterOrDelay.Unit.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def repeaterOrDelay: P[RepeaterOrDelay] =
      (repeaterMark ~ repeaterValue ~ repeaterUnit).map { case (factory, value, unit) => factory(value, unit) }

    def timestamp: P[Timestamp] =
      (
        diary
        | activeTimestampRange
        | activeTimestamp
        | inactiveTimestampRange
        | inactiveTimestamp
      )

    def activeTimestamp: P[ActiveTimestamp] =
      (P("<") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P(">")).map { case (d, t, r) =>
        ActiveTimestamp(d, t, r)
      }

    def inactiveTimestamp: P[InactiveTimestamp] =
      (P("[") ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ P("]")).map { case (d, t, r) =>
        InactiveTimestamp(d, t, r)
      }

    def activeTimestampRange: P[ActiveTimestampRange] =
      (
        (activeTimestamp ~ P("-").rep(min = 1, max = 3).!! ~ activeTimestamp).map {
          case (from: ActiveTimestamp, to: ActiveTimestamp) => ActiveTimestampRange(from, to)
        }
        | P(P("<") ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P(">")).map { case (d, t1, t2, r) =>
          ActiveTimestampRange(ActiveTimestamp(d, Some(t1), r), t2)
        }
      )

    def inactiveTimestampRange: P[InactiveTimestampRange] =
      (
        (inactiveTimestamp ~ P("-").rep(min = 1, max = 3).!! ~ inactiveTimestamp).map {
          case (from: InactiveTimestamp, to: InactiveTimestamp) => InactiveTimestampRange(from, to)
        }
        |
        (P("[") ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ P("]")).map { case (d, t1, t2, r) =>
          InactiveTimestampRange(InactiveTimestamp(d, Some(t1), r), t2)
        }
      )
  }

  private[org4s] object planning {
    private def info[A <: Planning.Info](keyword: String, f: Timestamp => A): P[A] =
      (s.rep().!! ~ P(keyword) ~ P(": ") ~ timestamp.timestamp ~ eolOrEnd).map(f)

    def deadlineInfo: P[Planning.Info.Deadline] =
      info("DEADLINE", Planning.Info.Deadline.apply)
    def scheduledInfo: P[Planning.Info.Scheduled] =
      info("SCHEDULED", Planning.Info.Scheduled.apply)
    def closedInfo: P[Planning.Info.Closed] =
      info("CLOSED", Planning.Info.Closed.apply)

    def planning: P[Planning] =
      (deadlineInfo | scheduledInfo | closedInfo).rep(1).map(_.toList).map(Planning.apply)
  }

  private[org4s] object keyword {
    import elements.Keyword._

    private def key[A](keyParser: => P[A]): P[A] = s0 ~ P("#+") ~ keyParser ~ P(":") ~ s0

    private def resultsKeyword: P[Affiliated.Results] =
      (key(P("RESULTS") ~ (P("[") ~ (!(eol | P("]")) ~ anyChar).rep(1).! ~ P("]")).?) ~ charsUntilEol ~ eolOrEnd).map {
        case (optional, value) => Affiliated.Results(value, optional)
      }

    private def content: P[Content] =
      timestamp.timestamp | link.link | markup.textMarkup | (!eol ~ singleCharText)

    private def captionKeyword: P[Affiliated.Caption] =
      (
        key(
          P("CAPTION") ~ (P("[") ~ (!P("]") ~ content).rep(1) ~ P("]")).?
        ).map(_.map(_.toList).map(foldTexts[Content]))
          ~ (!P("]") ~ content)
            .rep(1)
            .map(_.toList)
            .map(foldTexts[Content])
          ~ eolOrEnd
      ).map { case (optional, value) => Affiliated.Caption(value, optional) }

    private def simpleValue: P[String] = charsUntilEol ~ eolOrEnd

    def affiliatedKeyword: P[Affiliated] =
      (
        (key(P("HEADER")) ~ simpleValue).map(Affiliated.Header.apply)
        | (key(P("NAME")) ~ simpleValue).map(Affiliated.Name.apply)
        | (key(P("PLOT")) ~ simpleValue).map(Affiliated.Plot.apply)
        | resultsKeyword
        | captionKeyword
      )

    def tableFormula: P[TableFormula] =
      (key(P("TBLFM")) ~ simpleValue).map(TableFormula.apply)

    def call: P[Call] =
      (key(P("CALL")) ~ simpleValue).map(Call.apply)

    def todo: P[Todo] = {
      def word: P[String] = fromRange("A-Za-z").rep(1).!
      (
        key(P("TODO"))
        ~ ((s0 ~ word).rep(1).? ~ (s0 ~ P("|") ~ (s ~ word).rep(1)).?)
      ).filter { case (o1, o2) => o1.isDefined || o2.isDefined }.map {
        case (Some(todoLs), Some(doneLs)) => Todo(todoLs.toList, doneLs.toList)
        case (Some(ls), None)             => Todo(ls.slice(0, ls.size - 1).toList, List(ls.last))
        case (None, Some(ls))             => Todo(Nil, ls.toList)
        case (None, None) =>
          sys.error(
            "Unexpected state: thrown because matched case should have been filtered in previous .filter(..) call"
          )
      }
    }

    private def genericKey: P[String] =
      !(affiliatedKeyword | tableFormula | call | todo) ~ key((!P(":") ~ anyChar).rep().!)

    def keyword: P[GenericKeyword] =
      for {
        k <- genericKey
        v <-
          if (ctx.elementDocumentProperties.contains(k)) {
            (content.rep(1) ~ eolOrEnd).map(_.toList).map(foldTexts[Content])
          } else {
            ((!eolOrEnd ~ anyChar).rep(1).! ~ eolOrEnd).map(Text.apply).map(List(_))
          }
      } yield GenericKeyword(k, v)
  }

  private[org4s] object table {
    import models.elements.TableRow
    import models.elements.TableRow._
    import models.greater_elements.Table
    import models.objects.TableCell

    def table: P[Table] = P(tableOrg)

    def tableOrg: P[Table] =
      (tableRow ~ eol ~ (tableRow ~ eol).rep() ~ (keyword.tableFormula ~ eol).rep()).map {
        case (firstRow, restRows, formulas) => Table(firstRow :: restRows.toList, formulas.toList)
      }

    def tableRow: P[TableRow] = s0 ~ (tableRowSep | tableRowCells)
    def tableRowSep: P[TableSep.type] = (P("|-") ~ anyFrom("\\-+|").rep()).map(_ => TableSep)

    def tableRowCells: P[TableRowCells] =
      (P("|") ~ tableCell ~ (P("|") ~ tableCell).rep() ~ P("|").?.!!).map {
        case (first: TableCell, rest: List[TableCell]) => TableRowCells((first :: rest.toList).filter(_.value.nonEmpty))
      }

    def tableCell: P[TableCell] = charsUntilIn("\n|").map(s => TableCell(s.trim))
  }

  private[org4s] object target {
    import models.objects.{ RadioTarget, Target }

    def radioTarget: P[RadioTarget] =
      (P("<<<") ~ !P(" ") ~ charsUntilIn("<>\n") ~ !P(" ") ~ P(">>>"))
        .map(RadioTarget.apply)

    def target: P[Target] =
      (P("<<") ~ !P(" ") ~ charsUntilIn("<>\n") ~ !P(" ") ~ P(">>"))
        .map(Target.apply)
  }

  private[org4s] object markup {

    import models.objects.{ Marker, TextMarkup }

    private def pre: P[String] = (anyFrom("\n\r \t\\-({\'\"")).!
    private def post: P[Unit] = end | anyFrom("\n\r \t\\-)}\'\".,:;!?[")

    private def marker: P[Marker] =
      anyFrom("*=/+_~").!.map(Marker.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def textMarkup: P[TextMarkup] =
      (pre.? ~ marker ~ !P(" "))
        .flatMap[TextMarkup] { case (pre, marker) =>
          P(
            !P(marker.toString)
            ~ (if (marker.isNestable)
                 (timestamp.timestamp
                 | markup.textMarkup
                 | (!P(marker.toString) ~ singleCharText))
                   .rep(1)
                   .map(_.toList)
                   .map(foldTexts[TextMarkup.Content])
               else
                 (!P(marker.toString) ~ singleCharText)
                   .rep(1)
                   .map(_.reduce(_ ++ _))
                   .map(List(_))).map(TextMarkup(pre.getOrElse(""), marker, _))
            ~ P(marker.toString)
            ~ &(post)
          )
        }
  }

  private[org4s] object link {
    import models.objects.Link
    import models.objects.Link.*

    def linkProtocol: P[Protocol] =
      (!P(":") ~ fromRange("a-zA-Z0-9"))
        .rep(1)
        .!
        .filter(ctx.linkTypes.contains)
        .map(Protocol.apply)

    private def contentsWithoutLinks: P[Contents] =
      (!P("]") ~ anyChar.!.map(Text.apply))
        .rep()
        .map(_.toList)
        .map(foldTexts[OrgObject])
        .map(ls => Contents(ls))

    object regular {
      import models.objects.Link.RegularLink._

      sealed trait Path3

      private def path4: P[String] = charsUntilIn("]")

      private def protocolLink: P[ProtocolLink] =
        (
          P("[[") ~ linkProtocol ~ P(":") ~ P("//").?.!! ~ path4 ~ P("]")
            ~ (P("[") ~ contentsWithoutLinks ~ P("]")).? ~ P("]")
        ).map { case (lp: Protocol, path: String, c: Option[Contents]) => ProtocolLink(lp, path, c) }

      def regularLink: P[RegularLink] =
        protocolLink
    }

    object plain {
      import models.objects.Link.PlainLink

      private def path2: P[String] =
        (!(eol | anyFrom("()<> \t")) ~ anyChar).rep(1).!

      def plainLink: P[PlainLink] =
        (linkProtocol ~ P(":") ~ P("//").?.!! ~ path2).map { case (protocol: Protocol, path2: String) =>
          PlainLink(protocol, path2)
        }
    }

    def link: P[Link] =
      regular.regularLink | plain.plainLink
  }

  private[org4s] object cookies {
    import models.objects.StatCookie

    def emptyPercentCookie: P[StatCookie.EmptyPercent.type] =
      P("[%]").map(_ => StatCookie.EmptyPercent)

    def percentCookie: P[StatCookie.Percent] =
      (P("[") ~ d.rep(1).! ~ P("%]"))
        .map(_.toInt)
        .filter(StatCookie.Percent.isValid)
        .map(StatCookie.Percent.apply)

    def emptyFractionalCookie: P[StatCookie.EmptyFractional.type] =
      P("[/]").map(_ => StatCookie.EmptyFractional)

    def fractionalCookie: P[StatCookie.Fractional] =
      (P("[") ~ d.rep(1).! ~ P("/") ~ d.rep(1).! ~ P("]")).map { case (s1, s2) => (s1.toInt, s2.toInt) }.filter {
        case (v1, v2) => StatCookie.Fractional.isValid(v1, v2)
      }.map { case (v1, v2) => StatCookie.Fractional(v1, v2) }

    def statCookie: P[StatCookie] =
      emptyPercentCookie | emptyFractionalCookie | percentCookie | fractionalCookie
  }

  private[org4s] object headline {
    import models.Headline.*

    private[org4s] def stars(fromLevel: Int = 1): P[Int] =
      P("*").rep(min = fromLevel, max = ctx.inlineTaskMinLevel - 1).!.map(_.length)

    private[org4s] def priority: P[Priority] =
      (P("[#") ~ fromRange("A-Z").! ~ P("]")).map(s => s.toList.head).map(Priority.apply)

    private[org4s] def comment: P[Unit] = P(ctx.headlineComment)

    // TODO: refactor
    def keyword: P[Headline.Keyword] =
      (!(P(" ") | eolOrEnd) ~ anyChar)
        .rep(1)
        .!
        .map(value => (value, ctx.todoKeywords.findSet(value)))
        .filter { case (_, setOpt) => setOpt.isDefined }
        .map { case (value, setOpt) =>
          Headline.Keyword.KWSet(setOpt.get.todo, setOpt.get.done).mapStringToKeyword(value)
        }

    def tags: P[List[String]] =
      (P(":") ~ choice(fromRange("a-z"), fromRange("A-Z"), fromRange("0-9"), anyFrom("%@#_"))
        .rep(1)
        .!
        .rep(min = 1, sep = Some(P(":"))) ~ P(":"))
        .map(_.toList)

    def title: P[Title] =
      (
        timestamp.timestamp
          | markup.textMarkup
          | cookies.statCookie
          | (!(eol | tags) ~ anyChar.!.map(Text.apply))
      ).rep(1)
        .map(_.toList)
        .map(foldTexts[Title.Content])
        .map(Title.apply)

    def headline(fromLevel: Int = 1): P[Headline] =
      (
        stars(fromLevel)
          ~ (s.!! ~ keyword).?
          ~ (s.!! ~ priority).?
          ~ (s.!! ~ comment.!).?
          ~ (s.!! ~ title).?
          ~ s0 ~ tags.?
          ~ s0
          ~ eolOrEnd
      ).map {
        case (
              stars: Int,
              keyword: Option[Headline.Keyword],
              priority: Option[Priority],
              comment: Option[Unit],
              title: Option[Title],
              tags: Option[List[String]]
            ) =>
          Headline(
            stars,
            keyword,
            priority,
            title,
            tags.getOrElse(Nil),
            hasCommentKeyword = comment.isDefined
          )
      }
  }

  private[org4s] object plainList {
    import models.greater_elements.PlainList._

    def counter: P[Counter] = (d.rep(1) | fromRange("a-zA-Z")).!.map(Counter.apply)

    def counterSet: P[Counter] = P("[@") ~ counter ~ P("]")

    def checkbox: P[Checkbox] =
      (
        P("[")
        ~ (P(" ").map(_ => Checkbox.Empty)
        | P("X").map(_ => Checkbox.Checked)
        | P("-").map(_ => Checkbox.Unchecked))
        ~ P("]")
      )

    def tag: P[String] =
      !(P(" :: ") | eolOrEnd) ~ anyChar.rep(1).! ~ P(" :: ")

    def orderedBullet: P[Bullet.Ordered] =
      (counter ~ anyFrom(".)").!).map { case (i, c) => Bullet.Ordered(i, c(0)) } ~ (s | eolOrEnd)

    def charBullet: P[Bullet.Character] =
      anyFrom("*+\\-").!.map(_(0)).map(Bullet.Character.apply) ~ (s | eolOrEnd)

    private def anyItemObject: P[Content] =
      timestamp.timestamp | link.link | markup.textMarkup | (!eol ~ singleCharText)

    private def anyItemElement(minIndent: Int, maxIndent: Int): P[Element] =
      (
        table.table
        | plainList(minIndent, maxIndent)
        | emptyLines(Some(1))
        | (!indentation(0, minIndent - 1) ~ paragraph.paragraph)
      )
    def foo: P[Any] = P("asd").!.~(P("asd").!).~(P("asd").!)
    private def indentation(minIndent: Int, maxIndent: Int): P[Int] =
      P(P(" ").rep(min = minIndent, max = maxIndent) ~ !P(" ")).!.map(_.length)

    def item(minIndent: Int, maxIndent: Int): P[Item] =
      for {
        lvl    <- indentation(minIndent, maxIndent)
        bullet <- charBullet | orderedBullet
        t <- (
          (counterSet ~ s).?
          ~ (checkbox ~ s).?
          ~ (tag ~ s).?
          ~ anyItemObject.rep().map(_.toList).map(foldTexts[Content])
          ~ (
            end.map(_ => None)
            | (eol ~ anyItemElement(lvl + 1, maxIndent)
              .rep()
              .map(_.toList)
              .map(foldParagraphs[Element])
              .?)
          )
        )
      } yield t match {
        case (counterSet, checkbox, tag, content, elements) =>
          Item(lvl, bullet, checkbox, counterSet, tag, content, elements.getOrElse(Nil))
      }

    def plainList(minIndent: Int = 0, maxIndent: Int = 48): P[PlainList] =
      for {
        head <- !headline.headline() ~ item(minIndent, maxIndent)
        tail <- (!headline.headline() ~ item(head.indentation, maxIndent)).rep()
      } yield PlainList.Simple(head :: tail.toList)
  }

  private[org4s] object propertyDrawer {
    import models.elements.NodeProperty
    import models.greater_elements.PropertyDrawer

    private def nodePropertyName: P[String] =
      P(":") ~ (!(P("END") | P("+") | P(":") | eol | end) ~ anyChar).rep(1).! ~ P("+").?.!! ~ P(":")
    private def nodePropertyValue: P[String] = charsUntilEol

    def nodeProperty: P[NodeProperty] =
      (
        nodePropertyName
          ~ s0
          ~ nodePropertyValue.?.map(_.filter(_.nonEmpty))
          ~ eol
      ).map { case (name: String, value: Option[String]) => NodeProperty(name, value) }

    def propertyDrawer: P[PropertyDrawer] =
      (s0 ~ P(":PROPERTIES:") ~ eol ~ (s0 ~ nodeProperty).rep() ~ s0 ~ P(":END:") ~ eolOrEnd)
        .map(_.toList)
        .map(PropertyDrawer.apply)
  }

  private[org4s] object paragraph {
    private def anyParagraphObject: P[OrgObject] =
      timestamp.timestamp | link.link | markup.textMarkup | lineBreak | (!eol ~ singleCharText)

    def paragraph: P[Paragraph] =
      (
        (!headline.headline() ~ anyParagraphObject).rep(1)
          ~ (end.map(_ => Text("")) | eol.!.map(Text.apply))
      ).map { case (ls, last) => ls.toList ++ List(last) }
        .map(foldTexts[OrgObject])
        .map(Paragraph.apply)
  }

  private[org4s] def lineBreak: P[LineBreak.type] =
    (P("""\\""") ~ anyFrom("\t ").rep() ~ eolOrEnd).map(_ => LineBreak)

  private[org4s] def duration: P[Duration] =
    (P("=>") ~ s ~ d.rep(1).! ~ P(":") ~ d.rep(min = 2, max = 2).!).map { case (h, m) => (h.toInt, m.toInt) }.map {
      case (h, m) => Duration(h, m)
    }

  private[org4s] def clock: P[Clock] =
    P("CLOCK:") ~ s0 ~ (
      timestamp.inactiveTimestamp.map(Clock.Simple.apply)
      | (timestamp.inactiveTimestampRange ~ s ~ duration).map { case (tr, d) =>
        Clock.WithDuration(tr, d)
      }
    )

  private def singleCharText: P[Text] = anyChar.!.map(Text.apply)

  private def anySectionElement: P[Element] =
    keyword.keyword | table.table | plainList.plainList() | emptyLines() | paragraph.paragraph

  private def anyDocumentSectionElement: P[Element] =
    keyword.todo | anySectionElement

  private def emptyLines(max: Option[Int] = None): P[EmptyLines] =
    max
      .map(v => eol.rep(min = 1, max = v) ~ !eol)
      .getOrElse(eol.rep(1))
      .!
      .map(s => EmptyLines(s.length))

  private[org4s] def section(fromLevel: Int = 1): P[Section] =
    for {
      headline       <- headline.headline(fromLevel)
      planning       <- planning.planning.?
      propertyDrawer <- propertyDrawer.propertyDrawer.?
      elements       <- anySectionElement.rep()
      childSections  <- section(headline.level + 1).rep()
    } yield {
      Section(
        headline,
        foldParagraphs[Element](elements.toList),
        childSections.toList,
        planning,
        propertyDrawer
      )
    }

  def document: P[Document] = {
    for {
      elements <- anyDocumentSectionElement.rep()
      newParser = new OrgParser(
        ctx.copy(todoKeywords =
          elements
            .filter(_.isInstanceOf[Keyword.Todo])
            .map(_.asInstanceOf[Keyword.Todo])
            .map(keyword => OrgContext.TodoKeywords.KWSet(keyword.todoKeywords, keyword.doneKeywords))
            .map(set => OrgContext.TodoKeywords(List(set)))
            .foldLeft(ctx.todoKeywords)(_ ++ _)
        )
      )
      sections <- newParser.section().rep() ~ end
    } yield Document(elements.toList, sections.toList)
  }
}
