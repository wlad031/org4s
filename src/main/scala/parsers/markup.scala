package dev.vgerasimov.scorg
package parsers

import models.markup._

import fastparse.NoWhitespace._
import fastparse._

object markup {

  private def pre[_ : P]: P[String] = P(Start | CharIn("\n\r \t\\-({\'\"")).!
  private def post[_ : P]: P[Unit] = P(End | CharIn("\n\r \t\\-)}\'\".,:;!?["))
  private def marker[_ : P]: P[Marker] =
    P(CharIn("*=/+_~")).!.map(Marker.fromString)
      .filter(_.isDefined)
      .map(_.get)

  def textMarkup[_ : P]: P[TextMarkup] =
    P(pre ~ marker)
      .flatMap[TextMarkup]({
        case (pre, marker) =>
          P(
            !marker.toString
            ~ (if (marker.isNestable) contents(marker.toString)
               else anyNotIn(marker.toString).!)
            ~ marker.toString
            ~ &(post)
          )
            .map({
              case contents: Contents =>
                TextMarkup(pre, marker, contents)
              case contents: String =>
                TextMarkup(pre, marker, Contents(Text(contents)))
            })
      })

  private def contents[_ : P](s: String): P[Contents] =
    P(textMarkup | (!s ~ AnyChar.!.map(Text.apply))).rep.map(Contents.withTextFold)

  def contents[_ : P]: P[Contents] =
    P(textMarkup | AnyChar.!.map(Text.apply)).rep.map(Contents.withTextFold)
}
