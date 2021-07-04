package dev.vgerasimov.scorg

import models.greater_elements.PlainList
import models.greater_elements.PlainList._
import models.objects.Text

import org.scalacheck.Gen
import org.scalacheck.Prop._

class PlainListParsersTest extends ParserCheckSuite {

  private val charBulletGen = Gen.oneOf(Seq('*', '-', '+'))

  test("PLAIN LIST should parse simple list") {
    forAllNoShrink(charBulletGen) { (charBullet: Char) =>
      {
        checkParser(
          parser.plainList.plainList()(_),
          s"""$charBullet first
           |$charBullet second
           |$charBullet third""".stripMargin,
          PlainList.Simple(
            List(
              Item(0, Bullet.Character(charBullet), None, None, None, List(Text("first")), Nil),
              Item(0, Bullet.Character(charBullet), None, None, None, List(Text("second")), Nil),
              Item(0, Bullet.Character(charBullet), None, None, None, List(Text("third")), Nil)
            )
          )
        )
      }
    }
  }

  test("PLAIN LIST should parse nested list") {
    forAllNoShrink(charBulletGen, charBulletGen) { (charBullet1: Char, charBullet2: Char) =>
    {
      checkParser(
        parser.plainList.plainList()(_),
        s"""$charBullet1 first
           |  $charBullet2 first 1
           |    $charBullet1 first 1 1
           |  $charBullet2 first 2
           |$charBullet1 second
           |$charBullet1 third
           |  $charBullet2 third 1""".stripMargin,
        PlainList.Simple(
          List(
            Item(0, Bullet.Character(charBullet1), None, None, None, List(Text("first")), List(
              PlainList.Simple(List(
                Item(2, Bullet.Character(charBullet2), None, None, None, List(Text("first 1")), List(
                  PlainList.Simple(List(Item(4, Bullet.Character(charBullet1), None, None, None, List(Text("first 1 1")), Nil)))
                )),
                Item(2, Bullet.Character(charBullet2), None, None, None, List(Text("first 2")), Nil),
              ))
            )),
            Item(0, Bullet.Character(charBullet1), None, None, None, List(Text("second")), Nil),
            Item(0, Bullet.Character(charBullet1), None, None, None, List(Text("third")), List(
              PlainList.Simple(List(
                Item(2, Bullet.Character(charBullet2), None, None, None, List(Text("third 1")), Nil),
              ))
            ))
          )
        )
      )
    }
    }
  }
}
