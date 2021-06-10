package dev.vgerasimov.scorg
package parsers

import models.cookies._

import fastparse.NoWhitespace._
import fastparse._

object cookies {

  def emptyPercentCookie[_ : P]: P[StatCookie.EmptyPercent.type] =
    P("[%]").map(_ => StatCookie.EmptyPercent)

  def percentCookie[_ : P]: P[StatCookie.Percent] =
    P("[" ~ digit.rep(1).! ~ "%]")
      .map(_.toInt)
      .filter(StatCookie.Percent.isValid)
      .map(StatCookie.Percent.apply)

  def emptyFractionalCookie[_ : P]: P[StatCookie.EmptyFractional.type] =
    P("[/]").map(_ => StatCookie.EmptyFractional)

  def fractionalCookie[_ : P]: P[StatCookie.Fractional] =
    P("[" ~ digit.rep(1).! ~ "/" ~ digit.rep(1).! ~ "]")
      .map({ case (s1, s2) => (s1.toInt, s2.toInt) })
      .filter({ case (v1, v2) => StatCookie.Fractional.isValid(v1, v2) })
      .map({ case (v1, v2) => StatCookie.Fractional(v1, v2) })

  def statCookie[_ : P]: P[StatCookie] =
    emptyPercentCookie | emptyFractionalCookie | percentCookie | fractionalCookie
}
