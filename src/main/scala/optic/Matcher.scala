package dev.vgerasimov.org4s
package optic

import models.{ Element, Headline, Section }

import scala.reflect.{ ClassTag, classTag }

trait Matcher[A] {
  def isMatch(a: A): Boolean

  def and(that: Matcher[A]): Matcher[A] =
    (a: A) => this.isMatch(a) && that.isMatch(a)

  def any: Matcher[List[A]] =
    (ls: List[A]) => ls.exists(this.isMatch)

  def option(matchEmpty: Boolean = false): Matcher[Option[A]] = {
    case Some(v) => this.isMatch(v)
    case None    => matchEmpty
  }
}

object Matcher {
  def of[A, B](f: A => B, matcher: Matcher[B]): Matcher[A] = (a: A) => matcher.isMatch(f(a))

  def dropAll[A]: Matcher[A] = (_: A) => false

  def is[A, B : ClassTag]: Matcher[A] =
    (a: A) => classTag[B].runtimeClass.isInstance(a)
}

object SectionMatcher {
  def byHeadline(headlineMatcher: Matcher[Headline]): Matcher[Section] = {
    Matcher.of[Section, Headline](_.headline, headlineMatcher)
  }

  def byChildSections(childSectionsMatcher: Matcher[List[Section]]): Matcher[Section] =
    (section: Section) => childSectionsMatcher.isMatch(section.childSections)

  def byElements(elementsMatcher: Matcher[List[Element]]): Matcher[Section] =
    (section: Section) => {
      elementsMatcher.isMatch(section.elements)
    }
}

object HeadlineMatcher {
  def byTitle(titleMatcher: Matcher[Option[Headline.Title]]): Matcher[Headline] =
    (headline: Headline) => titleMatcher.isMatch(headline.title)

  def isComment(expected: Boolean = false): Matcher[Headline] =
    (headline: Headline) => headline.hasCommentKeyword == expected
}

object HeadlineTitleMatcher {
  private val toSearchableStringConverter =
    implicitly[ToSearchableStringConverter[Headline.Title]]

  def caseInsensitiveSubstring(toMatch: String): Matcher[Headline.Title] =
    (title: Headline.Title) =>
      toSearchableStringConverter.toPlainString(title).toLowerCase.contains(toMatch.toLowerCase())
}
