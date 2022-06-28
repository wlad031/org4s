package dev.vgerasimov.org4s
package optic

import models.{ Document, Element, Section }

import scala.reflect.ClassTag

trait Mapper[A, B] {
  def map(a: A): B

  def andThen[C](that: Mapper[B, C]): Mapper[A, C] = (a: A) => that.map(this.map(a))

  def list: Mapper[List[A], List[B]] = (ls: List[A]) => ls.map(this.map)
}

object Mapper {
  def none[A, B]: Mapper[A, Option[B]] = (_: A) => None

  def as[A, B : ClassTag]: Mapper[A, Option[B]] =
    (a: A) => Option.when(Matcher.is[A, B].isMatch(a))(a.asInstanceOf[B])
}

object DocumentMapper {
  def headSection: Mapper[Document, Option[Section]] =
    (document: Document) => document.sections.headOption
}

object SectionMapper {
  def headElement: Mapper[Section, Option[Element]] =
    (section: Section) => section.elements.headOption
}
