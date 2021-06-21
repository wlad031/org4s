package dev.vgerasimov.scorg
package parsers

import models.linebreak._

import fastparse.NoWhitespace._
import fastparse._

object linebreak {

  def lineBreak[_: P]: P[LineBreak.type] =
    P("""\\""" ~ CharIn("\t ").rep ~ (End | "\n")).map(_ => LineBreak)
}
