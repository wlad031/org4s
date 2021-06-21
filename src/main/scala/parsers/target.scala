package dev.vgerasimov.scorg
package parsers

import models.target._

import fastparse.NoWhitespace._
import fastparse._

object target {

  def radioTarget[_:P]: P[RadioTarget] =
    P("<<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>>")
      .map(RadioTarget.apply)

  def target[_:P]: P[Target] =
    P("<<" ~ !" " ~ anyNotIn("<>\n") ~ !" " ~ ">>")
      .map(Target.apply)
}
