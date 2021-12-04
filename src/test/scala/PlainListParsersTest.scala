package dev.vgerasimov.org4s

import models.elements.{ EmptyLines, Paragraph }
import models.greater_elements.PlainList
import models.greater_elements.PlainList._
import models.objects.{ Contents, Link, Text, TextMarkup }
import ops.table._

import org.scalacheck.Gen
import org.scalacheck.Prop._

class PlainListParsersTest extends ParserCheckSuite {

  private val charBulletGen = Gen.oneOf(Seq('*', '-', '+'))

  test("PLAIN LIST should parse simple list") {
    forAllNoShrink(charBulletGen) { (charBullet: Char) =>
      {
        checkParser(
          parser.plainList.plainList()(_),
          s""" $charBullet first
           | $charBullet second
           | $charBullet third""".stripMargin,
          PlainList.Simple(
            List(
              Item(1, Bullet.Character(charBullet), contents = List(Text("first"))),
              Item(1, Bullet.Character(charBullet), contents = List(Text("second"))),
              Item(1, Bullet.Character(charBullet), contents = List(Text("third")))
            )
          )
        )
      }
    }
  }

  test("PLAIN LIST should stop after two empty lines") {
    checkParser(
      parser.plainList.plainList()(_),
      s""" + first
         | + second
         |
         |
         | + third
         | + fourth
         |""".stripMargin,
      PlainList.Simple(
        List(
          Item(1, Bullet.Character('+'), contents = List(Text("first"))),
          Item(1, Bullet.Character('+'), contents = List(Text("second"))),
        )
      )
    )
  }

  test("PLAIN LIST should stop after lower item indentation") {
    checkParser(
      parser.plainList.plainList()(_),
      s"""  + first
         |  + second
         | + third
         | + fourth
         |""".stripMargin,
      PlainList.Simple(
        List(
          Item(2, Bullet.Character('+'), contents = List(Text("first"))),
          Item(2, Bullet.Character('+'), contents = List(Text("second"))),
        )
      )
    )
  }

  test("PLAIN LIST should stop after lower or equal not-item indentation") {
    checkParser(
      parser.plainList.plainList()(_),
      s""" + first
         | + second
         | text
         | + third
         | + fourth
         |""".stripMargin,
      PlainList.Simple(
        List(
          Item(1, Bullet.Character('+'), contents = List(Text("first"))),
          Item(1, Bullet.Character('+'), contents = List(Text("second"))),
        )
      )
    )
  }

  test("PLAIN LIST should parse nested list") {
    forAllNoShrink(charBulletGen, charBulletGen) { (charBullet1: Char, charBullet2: Char) =>
      {
        checkParser(
          parser.plainList.plainList()(_),
          s""" $charBullet1 first
           |   $charBullet2 first 1
           |     $charBullet1 first 1 1
           |   $charBullet2 first 2
           | $charBullet1 second
           | $charBullet1 third
           |   $charBullet2 third 1""".stripMargin,
          PlainList.Simple(
            List(
              Item(
                1,
                Bullet.Character(charBullet1),
                contents = List(Text("first")),
                elements = List(
                  PlainList.Simple(
                    List(
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("first 1")),
                        elements = List(
                          PlainList.Simple(
                            List(
                              Item(
                                5,
                                Bullet.Character(charBullet1),
                                contents = List(Text("first 1 1"))
                              )
                            )
                          )
                        )
                      ),
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("first 2"))
                      )
                    )
                  )
                )
              ),
              Item(1, Bullet.Character(charBullet1), contents = List(Text("second"))),
              Item(
                1,
                Bullet.Character(charBullet1),
                contents = List(Text("third")),
                elements = List(
                  PlainList.Simple(
                    List(
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("third 1"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  }

  test("PLAIN LIST should parse nested list with some elements") {
    forAllNoShrink(charBulletGen, charBulletGen) { (charBullet1: Char, charBullet2: Char) =>
      {
        checkParser(
          parser.plainList.plainList()(_),
          s""" $charBullet1 first
           |  This is an _underlined_ text of "first"
           |   $charBullet2 first 1
           |
           |    Multiline
           |    text
           |    of "first 1"
           |
           |    With two paragraphs
           |    even!
           |
           |     $charBullet1 first 1 1
           |      | A | B |
           |      | 1 | 2 |
           |
           |   $charBullet2 first 2
           | $charBullet1 second
           |  And this one with a [[https:vgerasimov.dev][link]].
           | $charBullet1 third
           |   $charBullet2 third 1""".stripMargin,
          PlainList.Simple(
            List(
              Item(
                1,
                Bullet.Character(charBullet1),
                contents = List(Text("first")),
                elements = List(
                  Paragraph(
                    List(
                      Text("  This is an"),
                      TextMarkup.underline(" ", "underlined"),
                      Text(" text of \"first\"\n")
                    )
                  ),
                  PlainList.Simple(
                    List(
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("first 1")),
                        elements = List(
                          EmptyLines(1),
                          Paragraph(
                            List(
                              Text(
                                """    Multiline
                                  |    text
                                  |    of "first 1"
                                  |""".stripMargin
                              )
                            )
                          ),
                          EmptyLines(1),
                          Paragraph(
                            List(
                              Text(
                                """    With two paragraphs
                                  |    even!
                                  |""".stripMargin
                              )
                            )
                          ),
                          EmptyLines(1),
                          PlainList.Simple(
                            List(
                              Item(
                                5,
                                Bullet.Character(charBullet1),
                                contents = List(Text("first 1 1")),
                                elements = List(
                                  ($("A") | $("B")) +
                                  ($("1") | $("2")),
                                  EmptyLines(1)
                                )
                              )
                            )
                          )
                        )
                      ),
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("first 2"))
                      )
                    )
                  )
                )
              ),
              Item(
                1,
                Bullet.Character(charBullet1),
                contents = List(Text("second")),
                elements = List(
                  Paragraph(
                    List(
                      Text("  And this one with a "),
                      Link.RegularLink.ProtocolLink(
                        Link.Protocol("https"),
                        "vgerasimov.dev",
                        Some(Contents(List(Text("link"))))
                      ),
                      Text(".\n")
                    )
                  )
                )
              ),
              Item(
                1,
                Bullet.Character(charBullet1),
                contents = List(Text("third")),
                elements = List(
                  PlainList.Simple(
                    List(
                      Item(
                        3,
                        Bullet.Character(charBullet2),
                        contents = List(Text("third 1"))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    }
  }
}
