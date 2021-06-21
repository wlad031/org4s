package dev.vgerasimov.scorg
package models

object affiliated {

  sealed trait AffiliatedKeyword
  object AffiliatedKeyword {
    case class Attr(backend: String, value: String) extends AffiliatedKeyword
    case class Caption(value: String, optional: Option[String]) extends AffiliatedKeyword
    case class Header(value: String) extends AffiliatedKeyword
    case class Name(value: String) extends AffiliatedKeyword
    case class Plot(value: String) extends AffiliatedKeyword
    case class Results(value: String, optional: Option[String]) extends AffiliatedKeyword
  }
}
