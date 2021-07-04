package dev.vgerasimov.scorg

import context._
import models._
import models.elements.{ EmptyLines, Paragraph, Planning }
import models.greater_elements.PlainList
import models.objects._
import ops.{ foldParagraphs, foldTexts }

import fastparse.NoWhitespace._
import fastparse.{ End, _ }

class OrgParser(implicit ctx: OrgContext) {

  def eol[_ : P]: P[Unit] = CharIn("\n\r")
  def eolOrEnd[_ : P]: P[Unit] = eol | End

  def anyNotIn[_ : P](string: String, min: Int = 1): P[String] =
    CharsWhile(c => !string.contains(c), min).!
  def anyNotNL[_ : P]: P[String] = anyNotIn("\n")
  def s[_ : P]: P[Unit] = CharIn("\t ").rep(1)
  def digit[_ : P]: P[Unit] = CharIn("0-9")

  object timestamp {
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

  def duration[_ : P]: P[Duration] =
    ("=>" ~ s ~ digit.rep(1).! ~ ":" ~ digit.rep(exactly = 2).!)
      .map({ case (h, m) => (h.toInt, m.toInt) })
      .map({ case (h, m) => Duration(h, m) })

  def clock[_ : P]: P[Clock] =
    "CLOCK:" ~ s.? ~ (
      timestamp.inactiveTimestamp.map(Clock.Simple.apply)
      | (timestamp.inactiveTimestampRange ~ s ~ duration).map({
        case (tr, d) => Clock.WithDuration(tr, d)
      })
    )

  object planning {
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

  def lineBreak[_ : P]: P[LineBreak.type] =
    ("""\\""" ~ CharIn("\t ").rep ~ eolOrEnd).map(_ => LineBreak)

  object keyword {
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

    private def genericKey[_ : P]: P[String] =
      !(affiliatedKeyword | tableFormula | call) ~ key((!":" ~ AnyChar).rep.!)

    def keyword[_ : P]: P[GenericKeyword] =
      for {
        k <- genericKey
        v <-
          if (ctx.elementDocumentProperties.contains(k)) {
            (content.rep(1) ~ eolOrEnd).map(_.toList).map(foldTexts[Content])
          } else {
            ((!eol ~ AnyChar).rep(1) ~ eolOrEnd).!.map(Text.apply).map(List(_))
          }
      } yield GenericKeyword(k, v)
  }

  object table {
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

  object target {
    import models.objects.{ RadioTarget, Target }

    def radioTarget[_ : P]: P[RadioTarget] =
      ("<<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>>")
        .map(RadioTarget.apply)

    def target[_ : P]: P[Target] =
      ("<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>")
        .map(Target.apply)
  }

  object markup {

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

  private[scorg] def anyObjectsWhileNot[_ : P](s: String): P[List[OrgObject]] =
    (timestamp.timestamp | markup.textMarkup | (!s ~ AnyChar.!.map(Text.apply)))
      .rep(1)
      .map(foldTexts[OrgObject])

  private[scorg] def anyObjects[_ : P]: P[List[OrgObject]] =
    (!headline.headline() ~ (timestamp.timestamp | markup.textMarkup | AnyChar.!.map(Text.apply)))
      .rep(1)
      .map(foldTexts[OrgObject])

  object link {
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
        ((!(eol | CharIn("(", ")", "<", ">", " ", "\t")) ~ AnyChar).rep(1).!)

      def plainLink[_ : P]: P[PlainLink] =
        (linkProtocol ~ ":" ~ "//".? ~ path2).map({ case (protocol, path2) => PlainLink(protocol, path2) })
    }

    def link[_ : P]: P[Link] =
      regular.regularLink | plain.plainLink
  }

  object cookies {
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

  object headline {
    import models.Headline._

    def stars[_ : P](fromLevel: Int = 1): P[Stars] =
      "*".rep(min = fromLevel, max = ctx.inlineTaskMinLevel - 1).!.map(s => Stars(s.length))

    def priority[_ : P]: P[Priority] =
      ("[#" ~ CharIn("A-Z").! ~ "]").map(s => s.toList.head).map(Priority)

    def commentH[_ : P]: P[Unit] = P(ctx.headlineComment)

    def keyword[_ : P]: P[Keyword] =
      ctx.todoKeywords._1
        .map(s => P(s).!.map(Keyword.Todo.apply))
        .reduce(_ | _) | ctx.todoKeywords._2
        .map(s => P(s).!.map(Keyword.Done.apply))
        .reduce(_ | _)

    def tags[_ : P]: P[Tags] =
      (":" ~ CharIn("a-zA-Z0-9%@#_").rep(1).!.rep(min = 1, sep = ":") ~ ":")
        .map(_.toList)
        .map(Tags)

    def title[_ : P]: P[Title] =
      (
        timestamp.timestamp
        | markup.textMarkup
        | cookies.statCookie
        | (!((s.? ~ eol) | (s ~ tags)) ~ AnyChar.!.map(Text.apply))
      ).rep
        .map(Title.withTextFold)

    def headline[_ : P](
      fromLevel: Int = 1
    ): P[Headline] =
      (
        stars(fromLevel)
        ~ (s ~ keyword).?
        ~ (s ~ priority).?
        ~ (s ~ commentH.!).?
        ~ (s ~ title).?
        ~ (s ~ tags).?
        ~ s.?
        ~ eolOrEnd
      ).map({
        case (stars, keyword, priority, comment, title, tags) =>
          Headline(
            stars,
            keyword,
            priority,
            title,
            tags,
            isComment = comment.isDefined
          )
      })
  }

  object plainList {
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
      table.table | plainList(minIndent, maxIndent) | emptyLines(Some(1)) | paragraph

    def item[_ : P](minIndent: Int, maxIndent: Int): P[Item] =
      for {
        lvl <- P(" ").rep(min = minIndent, max = maxIndent).!.map(_.length)
        t <- (
            (charBullet | orderedBullet)
            ~ (counterSet ~ s).?
            ~ (checkbox ~ s).?
            ~ (tag ~ s).?
            ~ anyItemObject.rep.map(foldTexts[Content])
            ~ (End.map(_ => Nil) | (eol ~ anyItemElement(lvl + 1, maxIndent).rep))
        )
      } yield t match {
        case (bullet, counterSet, checkbox, tag, content, elements) =>
          Item(lvl, bullet, checkbox, counterSet, tag, content, elements.toList)
      }

    def plainList[_ : P](minIndent: Int = 0, maxIndent: Int = 32): P[PlainList] =
      (!headline.headline() ~ item(minIndent, maxIndent)).rep(1).map(_.toList).map(PlainList.Simple.apply)
  }

  object propertyDrawer {
    import models.elements.NodeProperty

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

  private def singleCharText[_ : P]: P[Text] = AnyChar.!.map(Text.apply)

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

  private def anySectionElement[_ : P]: P[Element] =
    keyword.keyword | table.table | plainList.plainList() | emptyLines() | paragraph

  def emptyLines[_ : P](max: Option[Int] = None): P[EmptyLines] =
    max
      .map(v => eol.rep(min = 1, max = v))
      .getOrElse(eol.rep(1))
      .!
      .map(s => EmptyLines(s.length))

  def section[_ : P](
    fromLevel: Int = 1
  ): P[Section] =
    for {
      hl       <- headline.headline(fromLevel)./
      planning <- planning.planning.?./
      pDrawer  <- propertyDrawer.propertyDrawer.?./
      elements <- anySectionElement.rep./
      childs   <- section(hl.stars.n + 1).rep
    } yield {
      Section(hl, foldParagraphs[Element](elements), childs.toList, planning, pDrawer)
    }

  def document[_ : P]: P[Document] =
    (Start ~ anySectionElement.rep ~/ section().rep ~ End).map({
      case (elements, sections) => Document(elements.toList, sections.toList)
    })

}
