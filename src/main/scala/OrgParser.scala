package dev.vgerasimov.scorg

import models._
import models.elements.{ EmptyLines, Keyword, Paragraph }
import models.greater_elements.PlainList
import models.objects._
import ops._

import fastparse.NoWhitespace._
import fastparse.{ End, _ }

class OrgParser(ctx: OrgContext = OrgContext.defaultCtx) {

  private def eol[_ : P]: P[Unit] = CharIn("\n\r")
  private def eolOrEnd[_ : P]: P[Unit] = eol | End
  private def anyNotIn[_ : P](string: String, min: Int = 1): P[String] =
    CharsWhile(c => !string.contains(c), min).!
  private def anyNotNL[_ : P]: P[String] = anyNotIn("\n")
  private def s[_ : P]: P[Unit] = CharIn("\t ").rep(1)
  private def digit[_ : P]: P[Unit] = CharIn("0-9")

  private[scorg] object timestamp {
    import models.objects.Timestamp.Date._
    import models.objects.Timestamp.Time._
    import models.objects.Timestamp._

    def minute[_ : P]: P[Minute] =
      digit
        .rep(exactly = 2)
        .!
        .map(_.toInt)
        .filter(Minute.isValid)
        .map(Minute.apply)

    def hour[_ : P]: P[Hour] =
      digit
        .rep(min = 1, max = 2)
        .!
        .map(_.toInt)
        .filter(Hour.isValid)
        .map(Hour.apply)

    def time[_ : P]: P[Time] =
      (hour ~ ":" ~ minute)
        .map({ case (hour, minute) => Time(hour, minute) })

    def year[_ : P]: P[Year] =
      digit
        .rep(exactly = 4)
        .!
        .map(_.toInt)
        .filter(Year.isValid)
        .map(Year.apply)

    def month[_ : P]: P[Month] =
      digit
        .rep(exactly = 2)
        .!
        .map(_.toInt)
        .filter(Month.isValid)
        .map(Month.apply)

    def day[_ : P]: P[Day] =
      digit
        .rep(exactly = 2)
        .!
        .map(_.toInt)
        .filter(Day.isValid)
        .map(Day.apply)

    def dayName[_ : P]: P[DayName] =
      anyNotIn("+-]> \t\n\r0123456789").!.map(DayName.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def date[_ : P]: P[Date] =
      (year ~ "-" ~ month ~ "-" ~ day ~ (" " ~ dayName).?)
        .filter({ case (year, month, day, _) => Date.isValid(year, month, day) })
        .map({ case (year, month, day, dayName) => Date(year, month, day, dayName) })

    def diary[_ : P]: P[Diary] = ("<%%(" ~ anyNotIn("\n>") ~ ")>").map(Diary.apply)

    def repeaterMark[_ : P]: P[(RepeaterOrDelay.Value, RepeaterOrDelay.Unit) => RepeaterOrDelay] =
      (
        P("++").map(_ => RepeaterOrDelay.CatchUpRepeater)
        | P("+").map(_ => RepeaterOrDelay.CumulateRepeater)
        | P(".+").map(_ => RepeaterOrDelay.RestartRepeater)
        | P("--").map(_ => RepeaterOrDelay.FirstTypeDelay)
        | P("-").map(_ => RepeaterOrDelay.AllTypeDelay)
      )

    def repeaterValue[_ : P]: P[RepeaterOrDelay.Value] =
      digit
        .rep(1)
        .!
        .map(_.toInt)
        .map(RepeaterOrDelay.Value.apply)

    def repeaterUnit[_ : P]: P[RepeaterOrDelay.Unit] =
      CharIn("hdwmy").!.map(RepeaterOrDelay.Unit.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def repeaterOrDelay[_ : P]: P[RepeaterOrDelay] =
      (repeaterMark ~ repeaterValue ~ repeaterUnit)
        .map({ case (factory, value, unit) => factory(value, unit) })

    def timestamp[_ : P]: P[Timestamp] =
      (
        diary
        | activeTimestampRange
        | activeTimestamp
        | inactiveTimestampRange
        | inactiveTimestamp
      )

    def activeTimestamp[_ : P]: P[ActiveTimestamp] =
      ("<" ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ ">")
        .map({ case (d, t, r) => ActiveTimestamp(d, t, r) })

    def inactiveTimestamp[_ : P]: P[InactiveTimestamp] =
      ("[" ~ date ~ (s ~ time).? ~ (s ~ repeaterOrDelay).? ~ "]")
        .map({ case (d, t, r) => InactiveTimestamp(d, t, r) })

    def activeTimestampRange[_ : P]: P[ActiveTimestampRange] =
      (
        (activeTimestamp ~ "-".rep(min = 1, max = 3) ~ activeTimestamp).map({
          case (from, to) => ActiveTimestampRange(from, to)
        })
        | P("<" ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ ">")
          .map({ case (d, t1, t2, r) => ActiveTimestampRange(ActiveTimestamp(d, Some(t1), r), t2) })
      )

    def inactiveTimestampRange[_ : P]: P[InactiveTimestampRange] =
      (
        (inactiveTimestamp ~ "-".rep(min = 1, max = 3) ~ inactiveTimestamp)
          .map({ case (from, to) => InactiveTimestampRange(from, to) })
        |
        ("[" ~ date ~ s ~ time ~ s ~ time ~ (s ~ repeaterOrDelay).? ~ "]")
          .map({
            case (d, t1, t2, r) => InactiveTimestampRange(InactiveTimestamp(d, Some(t1), r), t2)
          })
      )
  }

  private[scorg] object planning {
    private def info[A <: Planning.Info, _ : P](keyword: String, f: Timestamp => A): P[A] =
      (s.rep ~ P(keyword) ~ ": " ~ timestamp.timestamp ~ eolOrEnd).map(f)

    def deadlineInfo[_ : P]: P[Planning.Info.Deadline] =
      info("DEADLINE", Planning.Info.Deadline.apply)
    def scheduledInfo[_ : P]: P[Planning.Info.Scheduled] =
      info("SCHEDULED", Planning.Info.Scheduled.apply)
    def closedInfo[_ : P]: P[Planning.Info.Closed] =
      info("CLOSED", Planning.Info.Closed.apply)

    def planning[_ : P]: P[Planning] =
      (deadlineInfo | scheduledInfo | closedInfo).rep(1).map(_.toList).map(Planning.apply)
  }

  private[scorg] object keyword {
    import elements.Keyword._

    private def key[A, _ : P](keyParser: => P[A]): P[A] = s.? ~ "#+" ~ keyParser ~ ":" ~ s.?

    private def resultsKeyword[_ : P]: P[Affiliated.Results] =
      (key(P("RESULTS") ~ ("[" ~ (!(eol | "]") ~ AnyChar).rep(1).! ~ "]").?) ~ anyNotNL ~ eolOrEnd)
        .map({ case (optional, value) => Affiliated.Results(value, optional) })

    private def content[_ : P]: P[Content] =
      timestamp.timestamp | link.link | markup.textMarkup | (!eol ~ singleCharText)

    private def captionKeyword[_ : P]: P[Affiliated.Caption] =
      (
        key(
          P("CAPTION") ~ ("[" ~ (!"]" ~ content).rep(1) ~ "]").?
        ).map(_.map(_.toList).map(foldTexts[Content]))
        ~ (!"]" ~ content)
          .rep(1)
          .map(_.toList)
          .map(foldTexts[Content])
        ~ eolOrEnd
      )
        .map({ case (optional, value) => Affiliated.Caption(value, optional) })

    private def simpleValue[_ : P]: P[String] = anyNotNL ~ eolOrEnd

    def affiliatedKeyword[_ : P]: P[Affiliated] =
      (
        (key(P("HEADER")) ~ simpleValue).map(Affiliated.Header.apply)
        | (key(P("NAME")) ~ simpleValue).map(Affiliated.Name.apply)
        | (key(P("PLOT")) ~ simpleValue).map(Affiliated.Plot.apply)
        | resultsKeyword
        | captionKeyword
      )

    def tableFormula[_ : P]: P[TableFormula] =
      (key(P("TBLFM")) ~ simpleValue).map(TableFormula.apply)

    def call[_ : P]: P[Call] =
      (key(P("CALL")) ~ simpleValue).map(Call.apply)

    def todo[_ : P]: P[Todo] = {
      def word: P[String] = CharIn("A-Za-z").rep(1).!
      (
        key(P("TODO"))
        ~ ((s.? ~ word).rep(1).? ~ (s.? ~ "|" ~ (s ~ word).rep(1)).?)
      )
        .filter({ case (o1, o2) => o1.isDefined || o2.isDefined })
        .map({
          case (Some(todoLs), Some(doneLs)) => Todo(todoLs.toList, doneLs.toList)
          case (Some(ls), None)             => Todo(ls.slice(0, ls.size - 1).toList, List(ls.last))
          case (None, Some(ls))             => Todo(Nil, ls.toList)
          case (None, None) =>
            sys.error(
              "Unexpected state: thrown because matched case should have been filtered in previous .filter(..) call"
            )
        })
    }

    private def genericKey[_ : P]: P[String] =
      !(affiliatedKeyword | tableFormula | call | todo) ~ key((!":" ~ AnyChar).rep.!)

    def keyword[_ : P]: P[GenericKeyword] =
      for {
        k <- genericKey
        v <-
          if (ctx.elementDocumentProperties.contains(k)) {
            (content.rep(1) ~ eolOrEnd).map(_.toList).map(foldTexts[Content])
          } else {
            ((!eolOrEnd ~ AnyChar).rep(1).! ~ eolOrEnd).map(Text.apply).map(List(_))
          }
      } yield GenericKeyword(k, v)
  }

  private[scorg] object table {
    import models.elements.TableRow
    import models.elements.TableRow._
    import models.greater_elements.Table
    import models.objects.TableCell

    def table[_ : P]: P[Table] = P(tableOrg)

    def tableOrg[_ : P]: P[Table] =
      (tableRow ~ eol ~ (tableRow ~ eol).rep ~ (keyword.tableFormula ~ eol).rep)
        .map({
          case (firstRow, restRows, formulas) => Table(firstRow :: restRows.toList, formulas.toList)
        })

    def tableRow[_ : P]: P[TableRow] = s.? ~ (tableRowSep | tableRowCells)
    def tableRowSep[_ : P]: P[TableSep.type] = ("|-" ~ CharIn("\\-+|").rep).map(_ => TableSep)

    def tableRowCells[_ : P]: P[TableRowCells] =
      ("|" ~ tableCell ~ ("|" ~ tableCell).rep ~ "|".?)
        .map({ case (first, rest) => TableRowCells(first :: rest.toList) })

    def tableCell[_ : P]: P[TableCell] = anyNotIn("\n|").map(s => TableCell(s.trim))
  }

  private[scorg] object target {
    import models.objects.{ RadioTarget, Target }

    def radioTarget[_ : P]: P[RadioTarget] =
      ("<<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>>")
        .map(RadioTarget.apply)

    def target[_ : P]: P[Target] =
      ("<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>")
        .map(Target.apply)
  }

  private[scorg] object markup {

    import models.objects.{ Marker, TextMarkup }

    private def pre[_ : P]: P[String] = (Start | CharIn("\n\r \t\\-({\'\"")).!
    private def post[_ : P]: P[Unit] = End | CharIn("\n\r \t\\-)}\'\".,:;!?[")

    private def marker[_ : P]: P[Marker] =
      CharIn("*=/+_~").!.map(Marker.fromString)
        .filter(_.isDefined)
        .map(_.get)

    def textMarkup[_ : P]: P[TextMarkup] =
      (pre ~ marker ~ !" ")
        .flatMap[TextMarkup]({
          case (pre, marker) =>
            P(
              !marker.toString
              ~ (if (marker.isNestable)
                   (timestamp.timestamp
                   | markup.textMarkup
                   | (!marker.toString ~ singleCharText))
                     .rep(1)
                     .map(_.toList)
                     .map(foldTexts[TextMarkup.Content])
                 else
                   (!marker.toString ~ singleCharText)
                     .rep(1)
                     .map(_.reduce(_ ++ _))
                     .map(List(_))).map(TextMarkup(pre, marker, _))
              ~ marker.toString
              ~ &(post)
            )
        })
  }

  private[scorg] object link {
    import models.objects.Link
    import models.objects.Link._

    def linkProtocol[_ : P]: P[Protocol] =
      (!":" ~ CharIn("a-zA-Z0-9"))
        .rep(1)
        .!
        .filter(ctx.linkTypes.contains)
        .map(Protocol.apply)

    private def contentsWithoutLinks[_ : P]: P[Contents] =
      (!"]" ~ AnyChar.!.map(Text.apply)).rep
        .map(_.toList)
        .map(foldTexts[OrgObject])
        .map(ls => Contents(ls))

    object regular {
      import models.objects.Link.RegularLink._

      sealed trait Path3

      private def path4[_ : P]: P[String] = anyNotIn("]")

      private def protocolLink[_ : P]: P[ProtocolLink] =
        (
          "[[" ~ linkProtocol ~ ":" ~ "//".? ~ path4 ~ "]"
          ~ ("[" ~ contentsWithoutLinks ~ "]").? ~ "]"
        )
          .map({ case (lp, path, c) => ProtocolLink(lp, path, c) })

      def regularLink[_ : P]: P[RegularLink] =
        protocolLink
    }

    object plain {
      import models.objects.Link.PlainLink

      private def path2[_ : P]: P[String] =
        (!(eol | CharIn("(", ")", "<", ">", " ", "\t")) ~ AnyChar).rep(1).!

      def plainLink[_ : P]: P[PlainLink] =
        (linkProtocol ~ ":" ~ "//".? ~ path2).map({
          case (protocol, path2) => PlainLink(protocol, path2)
        })
    }

    def link[_ : P]: P[Link] =
      regular.regularLink | plain.plainLink
  }

  private[scorg] object cookies {
    import models.objects.StatCookie

    def emptyPercentCookie[_ : P]: P[StatCookie.EmptyPercent.type] =
      P("[%]").map(_ => StatCookie.EmptyPercent)

    def percentCookie[_ : P]: P[StatCookie.Percent] =
      ("[" ~ digit.rep(1).! ~ "%]")
        .map(_.toInt)
        .filter(StatCookie.Percent.isValid)
        .map(StatCookie.Percent.apply)

    def emptyFractionalCookie[_ : P]: P[StatCookie.EmptyFractional.type] =
      P("[/]").map(_ => StatCookie.EmptyFractional)

    def fractionalCookie[_ : P]: P[StatCookie.Fractional] =
      ("[" ~ digit.rep(1).! ~ "/" ~ digit.rep(1).! ~ "]")
        .map({ case (s1, s2) => (s1.toInt, s2.toInt) })
        .filter({ case (v1, v2) => StatCookie.Fractional.isValid(v1, v2) })
        .map({ case (v1, v2) => StatCookie.Fractional(v1, v2) })

    def statCookie[_ : P]: P[StatCookie] =
      emptyPercentCookie | emptyFractionalCookie | percentCookie | fractionalCookie
  }

  private[scorg] object headline {
    import models.Headline._

    private[scorg] def stars[_ : P](fromLevel: Int = 1): P[Int] =
      "*".rep(min = fromLevel, max = ctx.inlineTaskMinLevel - 1).!.map(_.length)

    private[scorg] def priority[_ : P]: P[Priority] =
      ("[#" ~ CharIn("A-Z").! ~ "]").map(s => s.toList.head).map(Priority)

    private[scorg] def comment[_ : P]: P[Unit] = P(ctx.headlineComment)

    // TODO: refactor
    def keyword[_ : P]: P[Headline.Keyword] =
      (!(" " | eolOrEnd) ~ AnyChar)
        .rep(1)
        .!
        .map(value => (value, ctx.todoKeywords.findSet(value)))
        .filter({ case (_, setOpt) => setOpt.isDefined })
        .map({
          case (value, setOpt) =>
            Headline.Keyword.KWSet(setOpt.get.todo, setOpt.get.done).mapStringToKeyword(value)
        })

    def tags[_ : P]: P[List[String]] =
      (":" ~ CharIn("a-zA-Z0-9%@#_").rep(1).!.rep(min = 1, sep = ":") ~ ":")
        .map(_.toList)

    def title[_ : P]: P[Title] =
      (
        timestamp.timestamp
        | markup.textMarkup
        | cookies.statCookie
        | (!(eol | tags) ~ AnyChar.!.map(Text.apply))
      ).rep(1)
        .map(_.toList)
        .map(foldTexts[Title.Content])
        .map(Title.apply)

    def headline[_ : P](fromLevel: Int = 1): P[Headline] =
      (
        stars(fromLevel)
        ~ (s ~ keyword).?
        ~ (s ~ priority).?
        ~ (s ~ comment.!).?
        ~ (s ~ title).?
        ~ s.? ~ tags.?
        ~ s.?
        ~ eolOrEnd
      ).map({
        case (stars, keyword, priority, comment, title, tags) =>
          Headline(
            stars,
            keyword,
            priority,
            title,
            tags.getOrElse(Nil),
            hasCommentKeyword = comment.isDefined
          )
      })
  }

  private[scorg] object plainList {
    import models.greater_elements.PlainList._

    def counter[_ : P]: P[Counter] = (digit.rep(1) | CharIn("a-zA-Z")).!.map(Counter.apply)

    def counterSet[_ : P]: P[Counter] = "[@" ~ counter ~ "]"

    def checkbox[_ : P]: P[Checkbox] =
      (
        "["
        ~ (P(" ").map(_ => Checkbox.Empty)
        | P("X").map(_ => Checkbox.Checked)
        | P("-").map(_ => Checkbox.Unchecked))
        ~ "]"
      )

    def tag[_ : P]: P[String] =
      !(" :: " | eolOrEnd) ~ AnyChar.rep(1).! ~ " :: "

    def orderedBullet[_ : P]: P[Bullet.Ordered] =
      (counter ~ CharIn(".)").!)
        .map({ case (i, c) => Bullet.Ordered(i, c(0)) }) ~ (s | eolOrEnd)

    def charBullet[_ : P]: P[Bullet.Character] =
      CharIn("*+\\-").!.map(_(0)).map(Bullet.Character.apply) ~ (s | eolOrEnd)

    private def anyItemObject[_ : P]: P[Content] =
      timestamp.timestamp | link.link | markup.textMarkup | (!eol ~ singleCharText)

    private def anyItemElement[_ : P](minIndent: Int, maxIndent: Int): P[Element] =
      (
        table.table
        | plainList(minIndent, maxIndent)
        | emptyLines(Some(1))
        | (!indentation(0, minIndent - 1) ~ paragraph.paragraph)
      )

    private def indentation[_ : P](minIndent: Int, maxIndent: Int): P[Int] =
      P(" ".rep(min = minIndent, max = maxIndent) ~ !" ").!.map(_.length)

    def item[_ : P](minIndent: Int, maxIndent: Int): P[Item] =
      for {
        lvl    <- indentation(minIndent, maxIndent)
        bullet <- charBullet | orderedBullet
        t <- (
            (counterSet ~ s).?
            ~ (checkbox ~ s).?
            ~ (tag ~ s).?
            ~ anyItemObject.rep.map(_.toList).map(foldTexts[Content])
            ~ (
              End.map(_ => None)
              | (eol ~ anyItemElement(lvl + 1, maxIndent).rep
                .map(_.toList)
                .map(foldParagraphs[Element])
                .?)
            )
        )
      } yield t match {
        case (counterSet, checkbox, tag, content, elements) =>
          Item(lvl, bullet, checkbox, counterSet, tag, content, elements.getOrElse(Nil))
      }

    def plainList[_ : P](minIndent: Int = 0, maxIndent: Int = 48): P[PlainList] =
      for {
        head <- !headline.headline() ~ item(minIndent, maxIndent)
        tail <- (!headline.headline() ~ item(head.indentation, maxIndent)).rep
      } yield PlainList.Simple(head :: tail.toList)
  }

  private[scorg] object propertyDrawer {
    import models.elements.NodeProperty
    import models.greater_elements.PropertyDrawer

    private def nodePropertyName[_ : P]: P[String] =
      ":" ~ (!("END" | CharIn("+:") | eolOrEnd) ~ AnyChar).rep(1).! ~ "+".? ~ ":"
    private def nodePropertyValue[_ : P]: P[String] = anyNotNL

    def nodeProperty[_ : P]: P[NodeProperty] =
      (
        nodePropertyName
        ~ s.?
        ~ nodePropertyValue.?
        ~ eol
      ).map({ case (name, value) => NodeProperty(name, value) })

    def propertyDrawer[_ : P]: P[PropertyDrawer] =
      (s.? ~ ":PROPERTIES:" ~ eol ~ (s.? ~ nodeProperty).rep ~ s.? ~ ":END:" ~ eolOrEnd)
        .map(_.toList)
        .map(PropertyDrawer.apply)
  }

  private[scorg] object paragraph {
    private def anyParagraphObject[_ : P]: P[OrgObject] =
      timestamp.timestamp | link.link | markup.textMarkup | lineBreak | (!eol ~ singleCharText)

    def paragraph[_ : P]: P[Paragraph] =
      (
        (!headline.headline() ~ anyParagraphObject).rep(1)
        ~ (End.map(_ => Text("")) | eol.!.map(Text.apply))
      )
        .map({ case (ls, last) => ls.toList ++ List(last) })
        .map(foldTexts[OrgObject])
        .map(Paragraph.apply)
  }

  private[scorg] def lineBreak[_ : P]: P[LineBreak.type] =
    ("""\\""" ~ CharIn("\t ").rep ~ eolOrEnd).map(_ => LineBreak)

  private[scorg] def duration[_ : P]: P[Duration] =
    ("=>" ~ s ~ digit.rep(1).! ~ ":" ~ digit.rep(exactly = 2).!)
      .map({ case (h, m) => (h.toInt, m.toInt) })
      .map({ case (h, m) => Duration(h, m) })

  private[scorg] def clock[_ : P]: P[Clock] =
    "CLOCK:" ~ s.? ~ (
      timestamp.inactiveTimestamp.map(Clock.Simple.apply)
      | (timestamp.inactiveTimestampRange ~ s ~ duration).map({
        case (tr, d) => Clock.WithDuration(tr, d)
      })
    )

  private def singleCharText[_ : P]: P[Text] = AnyChar.!.map(Text.apply)

  private def anySectionElement[_ : P]: P[Element] =
    keyword.keyword | table.table | plainList.plainList() | emptyLines() | paragraph.paragraph

  private def anyDocumentSectionElement[_ : P]: P[Element] =
    keyword.todo | anySectionElement

  private def emptyLines[_ : P](max: Option[Int] = None): P[EmptyLines] =
    max
      .map(v => eol.rep(min = 1, max = v) ~ !eol)
      .getOrElse(eol.rep(1))
      .!
      .map(s => EmptyLines(s.length))

  private[scorg] def section[_ : P](fromLevel: Int = 1): P[Section] =
    for {
      headline       <- headline.headline(fromLevel)./
      planning       <- planning.planning.?./
      propertyDrawer <- propertyDrawer.propertyDrawer.?./
      elements       <- anySectionElement.rep./
      childSections  <- section(headline.level + 1).rep
    } yield {
      Section(
        headline,
        foldParagraphs[Element](elements.toList),
        childSections.toList,
        planning,
        propertyDrawer
      )
    }

  def document[_ : P]: P[Document] = {
    for {
      elements <- Start ~ anyDocumentSectionElement.rep
      newParser = new OrgParser(
        ctx.copy(todoKeywords =
          elements
            .filter(_.isInstanceOf[Keyword.Todo])
            .map(_.asInstanceOf[Keyword.Todo])
            .map(keyword =>
              OrgContext.TodoKeywords.KWSet(keyword.todoKeywords, keyword.doneKeywords)
            )
            .map(set => OrgContext.TodoKeywords(List(set)))
            .foldLeft(ctx.todoKeywords)(_ ++ _)
        )
      )
      sections <- newParser.section().rep ~ End
    } yield Document(elements.toList, sections.toList)
  }
}
