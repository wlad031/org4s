package dev.vgerasimov.scorg

import models.objects.Link._
import models.objects.{ Contents, Link, Text }

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class LinkParsersTest extends ParserCheckSuite {

  test("LINK PROTOCOL should parse any known protocol") {
    forAll(Gen.oneOf(ctx.linkTypes)) { (linkType: String) =>
      {
        checkParser(
          parser.link.linkProtocol(_),
          linkType,
          Protocol(linkType)
        )
      }
    }
  }

  test("LINK PROTOCOL should not parse unknown protocol") {
    checkParserFailed(
      parser.link.linkProtocol(_),
      "unknown"
    )
  }

  test("LINK should parse simple link without description") {
    checkParser(
      parser.link.link(_),
      """[[https://ru.wikipedia.org/wiki/%D0%98%D0%B5%D1%80%D0%B0%D1%80%D1%85%D0%B8%D1%8F_%D0%A5%D0%BE%D0%BC%D1%81%D0%BA%D0%BE%D0%B3%D0%BE]]
        |""".stripMargin,
      Link.RegularLink.ProtocolLink(
        Protocol("https"),
        "ru.wikipedia.org/wiki/%D0%98%D0%B5%D1%80%D0%B0%D1%80%D1%85%D0%B8%D1%8F_%D0%A5%D0%BE%D0%BC%D1%81%D0%BA%D0%BE%D0%B3%D0%BE"
      )
    )
  }

  test("LINK should parse simple link with plain description") {
    checkParser(
      parser.link.link(_),
      "[[https://vgerasimov.dev][My Blog]]",
      Link.RegularLink.ProtocolLink(
        Protocol("https"),
        "vgerasimov.dev",
        Some(Contents(Text("My Blog")))
      )
    )
  }
}
