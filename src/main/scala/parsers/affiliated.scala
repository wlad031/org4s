package dev.vgerasimov.scorg
package parsers

import models.affiliated._

import fastparse.NoWhitespace._
import fastparse._

object affiliated {

  def attrAKW[_: P]: P[AffiliatedKeyword.Attr] =
    P("#+ATTR_" ~ CharIn("\\-_a-zA-Z0-9").! ~ ":" ~ s ~ anyNotNL)
      .map({ case (b, v) => AffiliatedKeyword.Attr(b, v) })

  def captionAKW[_ : P]: P[AffiliatedKeyword.Caption] =
    affiliatedKeywordWithOptional("CAPTION")
      .map({ case (o, v) => AffiliatedKeyword.Caption(v, o) })

  def headerAKW[_ : P]: P[AffiliatedKeyword.Header] =
    affiliatedKeyword("HEADER").map(AffiliatedKeyword.Header.apply)

  def nameAKW[_ : P]: P[AffiliatedKeyword.Name] =
    affiliatedKeyword("NAME").map(AffiliatedKeyword.Name.apply)

  def plotAKW[_ : P]: P[AffiliatedKeyword.Plot] =
    affiliatedKeyword("PLOT").map(AffiliatedKeyword.Plot.apply)

  def resultsAKW[_ : P]: P[AffiliatedKeyword.Results] =
    affiliatedKeywordWithOptional("RESULTS")
      .map({ case (o, v) => AffiliatedKeyword.Results(v, o) })

  private def affiliatedKeyword[_ : P](str: String): P[String] =
    P("#+" ~ str ~ ":" ~ s ~ anyNotNL)

  private def affiliatedKeywordWithOptional[_ : P](str: String): P[(Option[String], String)] =
    P("#+" ~ str ~ ("[" ~ anyNotIn("]\n") ~ "]").? ~ ":" ~ s ~ anyNotNL)
}
