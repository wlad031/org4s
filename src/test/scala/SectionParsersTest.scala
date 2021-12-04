package dev.vgerasimov.org4s

import models.Headline._
import models.elements.{ EmptyLines, NodeProperty, Paragraph }
import models.greater_elements.PropertyDrawer
import models.objects.Timestamp.Date.{ Day, DayName, Month, Year }
import models.objects.Timestamp.Time.{ Hour, Minute }
import models.objects.Timestamp.{ Date, Time }
import models.objects.{ Text, TextMarkup, Timestamp }
import models.{ Headline, Planning, Section }
import ops.table._

class SectionParsersTest extends ParserCheckSuite {

  test("PARAGRAPH should parse single word") {
    checkParser(
      parser.paragraph.paragraph(_),
      "hello",
      Paragraph(List(Text("hello")))
    )
  }

  test("PARAGRAPH should parse single line text") {
    checkParser(
      parser.paragraph.paragraph(_),
      "hello awesome world!",
      Paragraph(List(Text("hello awesome world!")))
    )
  }

  test("PARAGRAPH should parse text until first newline character") {
    checkParser(
      parser.paragraph.paragraph(_),
      """hello
        |world
        |!""".stripMargin,
      Paragraph(List(Text("hello\n")))
    )
  }

  test("SECTION should parse <headline><no-empty-line><paragraph><empty-line>") {
    checkParser(
      parser.section()(_),
      """* H1
        |hello
        |
        |""".stripMargin,
      Section(
        Headline(1, title = Some(Title("H1"))),
        List(Paragraph(List(Text("hello\n"))), EmptyLines(1))
      )
    )
  }

  test("SECTION should parse <headline><empty-line><paragraph><empty-line>") {
    checkParser(
      parser.section()(_),
      """* H1
        |
        |hello
        |""".stripMargin,
      Section(
        Headline(1, title = Some(Title("H1"))),
        List(EmptyLines(1), Paragraph(List(Text("hello\n"))))
      )
    )
  }

  test("SECTION should parse <headline><empty-line><paragraph><2-empty-lines>") {
    checkParser(
      parser.section()(_),
      """* H1
        |
        |hello
        |
        |""".stripMargin,
      Section(
        Headline(1, title = Some(Title("H1"))),
        List(EmptyLines(1), Paragraph(List(Text("hello\n"))), EmptyLines(1))
      )
    )
  }

  test("SECTION should parse headline with property drawer") {
    checkParser(
      parser.section()(_),
      """* H1
        |:PROPERTIES:
        |:ID:       BA881640-C289-4305-8CC6-464CEA23AA77
        |:END:
        |
        |hello
        |
        |""".stripMargin,
      Section(
        Headline(1, title = Some(Title("H1"))),
        List(EmptyLines(1), Paragraph(List(Text("hello\n"))), EmptyLines(1)),
        propertyDrawer = Some(
          PropertyDrawer(List(NodeProperty("ID", Some("BA881640-C289-4305-8CC6-464CEA23AA77"))))
        )
      )
    )
  }

  test("SECTION should parse <headline><empty-line><table><2-empty-lines>") {
    checkParser(
      parser.section()(_),
      """* H1
        |
        || A | B | C |
        ||---+---+---|
        || 1 | 2 | 3 |
        |
        |""".stripMargin,
      Section(
        Headline(1, title = Some(Title("H1"))),
        List(
          EmptyLines(1),
          ($("A") | $("B") | $("C")) +
          sep +
          ($("1") | $("2") | $("3")),
          EmptyLines(1)
        ),
        Nil
      )
    )
  }

  test("SECTION should parse section with few random paragraphs") {
    checkParser(
      parser.section()(_),
      """** DONE This is the =heading= with tag :tag:
        |CLOSED: [2020-10-10 Sat 16:56]
        |
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean eget neque augue.
        |Nullam tempus orci ac ipsum efficitur, ut vehicula risus bibendum. Quisque condimentum
        |mauris eget neque pretium, vitae molestie arcu lobortis. Phasellus sodales mi sit amet
        |neque tempor, a tempus tortor imperdiet.
        |
        |*** First subheading
        |
        |Curabitur vel interdum felis. Curabitur ante libero, porttitor ut libero in, pretium
        |porttitor ipsum. Donec porta blandit volutpat. Nunc non sem est.
        |
        |*** [#A] Second subheading
        |
        |Suspendisse potenti. Nulla sed interdum sem. Sed ut justo tristique, posuere justo vel,
        |interdum metus. Proin ultricies fermentum turpis dignissim condimentum.
        |
        |**** Subsubheading :fun:lol:
        |
        |Duis dictum leo nunc, ac luctus augue iaculis tristique. Aenean a nulla condimentum,
        |vulputate quam ut, facilisis nulla. Cras eget lectus aliquam, mollis dolor a,
        |commodo sapien. Aenean dictum laoreet congue.
        |
        |*** Third subheading
        |
        |Ut a pulvinar ex, ut vehicula turpis. Duis rhoncus dui molestie purus hendrerit molestie.
        |Nulla aliquam dictum nulla, eu eleifend diam molestie eget. Praesent nec facilisis mauris.
        |
        |""".stripMargin,
      Section(
        headline = Headline(
          2,
          Some(Keyword.Done("DONE", Keyword.KWSet(List("TODO"), List("DONE")))),
          None,
          Some(
            Title(
              List(Text("This is the"), TextMarkup.verbatim(" ", "heading"), Text(" with tag "))
            )
          ),
          List("tag")
        ),
        planning = Some(
          Planning(
            List(
              Planning.Info.Closed(
                Timestamp.InactiveTimestamp(
                  Date(Year(2020), Month(10), Day(10), Some(DayName.Saturday)),
                  Some(Time(Hour(16), Minute(56)))
                )
              )
            )
          )
        ),
        elements = List(
          EmptyLines(1),
          Paragraph(
            List(
              Text(
                """Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean eget neque augue.
                  |Nullam tempus orci ac ipsum efficitur, ut vehicula risus bibendum. Quisque condimentum
                  |mauris eget neque pretium, vitae molestie arcu lobortis. Phasellus sodales mi sit amet
                  |neque tempor, a tempus tortor imperdiet.
                  |""".stripMargin
              )
            )
          ),
          EmptyLines(1)
        ),
        childSections = List(
          Section(
            Headline(3, title = Some(Title("First subheading"))),
            elements = List(
              EmptyLines(1),
              Paragraph(
                List(
                  Text(
                    """Curabitur vel interdum felis. Curabitur ante libero, porttitor ut libero in, pretium
                      |porttitor ipsum. Donec porta blandit volutpat. Nunc non sem est.
                      |""".stripMargin
                  )
                )
              ),
              EmptyLines(1)
            )
          ),
          Section(
            Headline(
              3,
              priority = Some(Priority('A')),
              title = Some(Title("Second subheading"))
            ),
            elements = List(
              EmptyLines(1),
              Paragraph(
                List(
                  Text(
                    """Suspendisse potenti. Nulla sed interdum sem. Sed ut justo tristique, posuere justo vel,
                      |interdum metus. Proin ultricies fermentum turpis dignissim condimentum.
                      |""".stripMargin
                  )
                )
              ),
              EmptyLines(1)
            ),
            childSections = List(
              Section(
                Headline(
                  4,
                  title = Some(Title("Subsubheading ")),
                  tags = List("fun", "lol")
                ),
                elements = List(
                  EmptyLines(1),
                  Paragraph(
                    List(
                      Text(
                        """Duis dictum leo nunc, ac luctus augue iaculis tristique. Aenean a nulla condimentum,
                          |vulputate quam ut, facilisis nulla. Cras eget lectus aliquam, mollis dolor a,
                          |commodo sapien. Aenean dictum laoreet congue.
                          |""".stripMargin
                      )
                    )
                  ),
                  EmptyLines(1)
                )
              )
            )
          ),
          Section(
            Headline(3, title = Some(Title("Third subheading"))),
            elements = List(
              EmptyLines(1),
              Paragraph(
                List(
                  Text(
                    """Ut a pulvinar ex, ut vehicula turpis. Duis rhoncus dui molestie purus hendrerit molestie.
                      |Nulla aliquam dictum nulla, eu eleifend diam molestie eget. Praesent nec facilisis mauris.
                      |""".stripMargin
                  )
                )
              ),
              EmptyLines(1)
            )
          )
        )
      )
    )
  }

  test("PROPERTY DRAWER should parse simple property drawer with single name-value") {
    checkParser(
      parser.propertyDrawer.propertyDrawer(_),
      """:PROPERTIES:
        |:ID:   hello-world
        |:END:""".stripMargin,
      PropertyDrawer(List(NodeProperty("ID", Some("hello-world"))))
    )
  }

  test("PROPERTY DRAWER should parse simple property drawer with single name") {
    checkParser(
      parser.propertyDrawer.propertyDrawer(_),
      """:PROPERTIES:
        |:ID:
        |:END:""".stripMargin,
      PropertyDrawer(List(NodeProperty("ID", None)))
    )
  }
}
