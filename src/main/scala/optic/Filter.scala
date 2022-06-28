package dev.vgerasimov.org4s
package optic

import models.{ Document, Element, Section }

trait Filter[A] {
  def filter(a: A): A

  def andThen(that: Filter[A]): Filter[A] = (a: A) => that.filter(this.filter(a))
}

object DocumentFilter {

  def sections(sectionMatcher: Matcher[Section]): Filter[Document] =
    (document: Document) =>
      document.copy(sections = document.sections.filter(sectionMatcher.isMatch))

  def elements(elementMatcher: Matcher[Element]): Filter[Document] =
    (document: Document) =>
      document.copy(elements = document.elements.filter(elementMatcher.isMatch))
}
