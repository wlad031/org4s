package dev.vgerasimov.org4s
package optic

import models.{ Headline, objects }
import models.objects.{ Text, TextMarkup }

trait ToSearchableStringConverter[A] {
  def toPlainString(a: A): String
}

object ToSearchableStringConverter {
  implicit val textToSearchableStringConverter: ToSearchableStringConverter[Text] = _.value

  implicit val textMarkupToSearchableStringConverter: ToSearchableStringConverter[TextMarkup] =
    _.contents
      .map({
        case x: Text => textToSearchableStringConverter.toPlainString(x)
        case x @ TextMarkup(pre, _, _) =>
          pre + textMarkupToSearchableStringConverter.toPlainString(x)
        case link: objects.Link           => ???
        case timestamp: objects.Timestamp => ???
      })
      .mkString

  implicit val titleToSearchableStringConverter: ToSearchableStringConverter[Headline.Title] =
    _.contents
      .map({
        case x: Text                      => textToSearchableStringConverter.toPlainString(x)
        case x: TextMarkup                => textMarkupToSearchableStringConverter.toPlainString(x)
        case link: objects.Link           => ???
        case timestamp: objects.Timestamp => ???
        case cookie: objects.StatCookie   => ""
      })
      .mkString
}
