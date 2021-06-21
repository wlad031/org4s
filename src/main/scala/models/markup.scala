package dev.vgerasimov.scorg
package models

object markup {

  sealed trait Marker {
    val isNestable: Boolean = this match {
      case Marker.Code     => false
      case Marker.Verbatim => false
      case _               => true
    }

    override def toString: String =
      this match {
        case Marker.Bold          => "*"
        case Marker.Verbatim      => "="
        case Marker.Italic        => "/"
        case Marker.StrikeThrough => "+"
        case Marker.Underline     => "_"
        case Marker.Code          => "~"
      }
  }

  object Marker {
    case object Bold extends Marker
    case object Verbatim extends Marker
    case object Italic extends Marker
    case object StrikeThrough extends Marker
    case object Underline extends Marker
    case object Code extends Marker

    def fromString(s: String): Option[Marker] =
      s match {
        case "*" => Some(Bold)
        case "=" => Some(Verbatim)
        case "/" => Some(Italic)
        case "+" => Some(StrikeThrough)
        case "_" => Some(Underline)
        case "~" => Some(Code)
        case _   => None
      }
  }

  sealed trait Content
  private[scorg] object Content {
    def text(value: String): Text = Text(value)
    def bold(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.Bold, Contents(List(Text(value))))
    def verb(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.Verbatim, Contents(List(Text(value))))
    def ital(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.Italic, Contents(List(Text(value))))
    def stri(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.StrikeThrough, Contents(List(Text(value))))
    def unde(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.Underline, Contents(List(Text(value))))
    def code(pre: String, value: String): TextMarkup =
      TextMarkup(pre, Marker.Code, Contents(List(Text(value))))
  }

  case class TextMarkup(pre: String, marker: Marker, value: Contents) extends Content
  object TextMarkup {
    def apply(pre: String, marker: Marker, text: String): TextMarkup =
      new TextMarkup(pre, marker, Contents(Text(text)))
  }
  case class Text(value: String) extends Content {
    def ++ (that: Text): Text = Text(this.value + that.value)
  }

  case class Contents(contents: List[Content])
  object Contents {
    def apply(contents: Content*): Contents = new Contents(contents.toList)

    def withTextFold(contents: Seq[Content]): Contents =
      Contents(
        contents
          .foldLeft[List[Content]](Nil)((ls, item) =>
            ls match {
              case Nil => List(item)
              case head :: tail =>
                (item, head) match {
                  case (t1: Text, t2: Text) => (t2 ++ t1) :: tail
                  case (t1, t2)             => t1 :: t2 :: tail
                }
            }
          )
          .reverse
      )
  }
}
