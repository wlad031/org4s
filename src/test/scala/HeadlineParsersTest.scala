package dev.vgerasimov.org4s

import models.Headline
import models.Headline._
import models.objects.Text
import models.objects.TextMarkup.italic

import fastparse._
import org.scalacheck.Gen
import org.scalacheck.Prop._

class HeadlineParsersTest extends ParserCheckSuite {

  private val starsGen: Gen[(Int, String)] =
    Gen.chooseNum(1, ctx.inlineTaskMinLevel - 1).map(n => (n, "*".repeat(n)))
  private val priorityGen: Gen[(Char, String)] = Gen.alphaUpperChar.map(c => (c, s"[#$c]"))
  private val tagGen: Gen[String] =
    Gen
      .someOf(List('%', '#', '@', '_') ++ ('a' to 'z') ++ ('A' to 'Z'))
      .map(_.mkString)
      .suchThat(_.nonEmpty)
  private val tagsGen: Gen[List[String]] = Gen.nonEmptyListOf(tagGen)

  test("STARS should not parse zero number of '*'") {
    val parsed = parse("", parser.headline.stars()(_))
    assert(!parsed.isSuccess)
  }

  test("STARS should parse any number of '*'") {
    forAll(starsGen) {
      case (n, starsString) =>
        val parsed = parse(starsString, parser.headline.stars()(_))
        parsed match {
          case Parsed.Success(value, _) => assertEquals(value, n)
          case r: Parsed.Failure        => fail(s"$starsString not parsed: $r")
        }
    }
  }

  test("PRIORITY should parse correct priority with any alpha character") {
    forAll(priorityGen) {
      case (char: Char, toParse: String) =>
        val parsed = parse(toParse, parser.headline.priority(_))
        parsed match {
          case Parsed.Success(value, _) => assertEquals(value, Priority(char))
          case r: Parsed.Failure        => fail(s"$toParse not parsed: $r")
        }
    }
  }

  test("TAGS should not parse not surrounded by ':' TAG") {
    forAll(tagGen) { (s: String) =>
      assert(!parse(s"$s", parser.headline.tags(_)).isSuccess)
      assert(!parse(s":$s", parser.headline.tags(_)).isSuccess)
      assert(!parse(s"$s:", parser.headline.tags(_)).isSuccess)
    }
  }

  test("TAGS should not parse empty tag") {
    assert(!parse("::", parser.headline.tags(_)).isSuccess)
  }

  test("TAGS should parse one tag surrounded by ':'") {
    forAllNoShrink(tagGen) { (tagString: String) =>
      val toParse = s":$tagString:"
      val parsed = parse(toParse, parser.headline.tags(_))
      parsed match {
        case Parsed.Success(value, _) => assertEquals(value, List(tagString))
        case r: Parsed.Failure        => fail(s"$toParse not parsed: $r")
      }
    }
  }

  test("TAGS should parse more than zero tags separated by ':'") {
    forAllNoShrink(tagsGen) { (tagStrings: List[String]) =>
      val toParse = tagStrings.mkString(":", ":", ":")
      val parsed = parse(toParse, parser.headline.tags(_))
      parsed match {
        case Parsed.Success(value, _) => assertEquals(value, tagStrings)
        case r: Parsed.Failure        => fail(s"$toParse not parsed: $r")
      }
    }
  }

  test("HEADLINE should not parse empty string") {
    assert(!parse("", parser.headline.headline()(_)).isSuccess)
  }

  test("HEADLINE should parse headline with only stars") {
    forAll(starsGen) {
      case (n, starsString) =>
        val toParse = starsString
        val parsed = parse(toParse, parser.headline.headline()(_))
        parsed match {
          case Parsed.Success(value, _) =>
            assertEquals(value, Headline(n))
          case r: Parsed.Failure => fail(s"$toParse not parsed: $r")
        }
    }
  }

  test("HEADLINE should parse simple headline with formatting") {
    val toParse = "* this is /italic/ headline"
    val parsed = parse(toParse, parser.headline.headline()(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          Headline(
            1,
            title = Some(Title(List(Text("this is"), italic(" ", "italic"), Text(" headline"))))
          )
        )
      case r: Parsed.Failure => fail(s"$toParse not parsed: $r")
    }
  }

  test("HEADLINE should parse headline with formatting and tags") {
    val toParse = "* this is /italic/ headline :tag1:tag2:"
    val parsed = parse(toParse, parser.headline.headline()(_))
    parsed match {
      case Parsed.Success(value, _) =>
        assertEquals(
          value,
          Headline(
            1,
            title = Some(Title(List(Text("this is"), italic(" ", "italic"), Text(" headline ")))),
            tags = List("tag1", "tag2")
          )
        )
      case r: Parsed.Failure => fail(s"$toParse not parsed: $r")
    }
  }

  test("HEADLINE should parse headline with different elements") {
    forAllNoShrink(
      starsGen,
      Gen.option(
        Gen.oneOf(
          OrgContext.default.todoKeywords.allValues
        )
      ),
      Gen.option(priorityGen),
      Gen.option(Gen.oneOf(Seq(OrgContext.default.headlineComment))),
      Gen.option(Gen.alphaNumStr.suchThat(_.nonEmpty)),
      Gen.option(tagsGen)
    ) {
      case ((n, starsString), optKeyword, optPriority, optComment, optTitle, optTags) =>
        val sb = new StringBuilder(starsString)
        // TODO: refactor
        def appendSome[A](o: Option[A]) =
          o match {
            case Some(value) => sb.append(" ").append(value)
            case None        => sb
          }
        appendSome(optKeyword)
        appendSome(optPriority.map(_._2))
        appendSome(optComment)
        appendSome(optTitle)
        appendSome(optTags.map(_.mkString(":", ":", ":")))
        val toParse = sb.toString()
        val parsed = parse(toParse, parser.headline.headline()(_))
        parsed match {
          case Parsed.Success(value, _) =>
            assertEquals(
              value,
              Headline(
                n,
                optKeyword.flatMap(s =>
                  OrgContext.default.todoKeywords.sets
                    .find(set => set.todo.contains(s))
                    .map(set => Keyword.Todo(s, Keyword.KWSet(set.todo, set.done)))
                    .orElse(
                      OrgContext.default.todoKeywords.sets
                        .find(set => set.done.contains(s))
                        .map(set => Keyword.Done(s, Keyword.KWSet(set.todo, set.done)))
                    )
                ),
                optPriority.map(_._1).map(Priority),
                optTitle.map(t => optTags.map(_ => t + " ").getOrElse(t)).map(Title.apply),
                optTags.getOrElse(Nil),
                hasCommentKeyword = optComment.isDefined
              )
            )
          case r: Parsed.Failure =>
            fail(s"$toParse not parsed: $r")
        }
    }
  }

}
