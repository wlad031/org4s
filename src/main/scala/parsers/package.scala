package dev.vgerasimov.scorg

import fastparse.NoWhitespace._
import fastparse._

/** Contains various generic parsers. */
package object parsers {

  def eol[_ : P]: P[Unit] = P("\r" | "\n")
  def emptyLine[_ : P]: P[EmptyLine.type] = P(" ".rep()).map(_ => EmptyLine)
  def anyNotIn[_ : P](string: String, min: Int = 1): P[String] =
    P(CharsWhile(c => !string.contains(c), min).!)
  def anyNotNL[_ : P]: P[String] = anyNotIn("\n")
  def anyNotWS[_ : P]: P[String] = anyNotIn(" \t")
  def s[_ : P]: P[Unit] = P(CharIn("\t ").rep(1))
  def digit[_ : P]: P[Unit] = P(CharIn("0-9"))
}
