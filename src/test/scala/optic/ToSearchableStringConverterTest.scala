package dev.vgerasimov.org4s
package optic

import models.Headline
import models.objects.{Marker, Text, TextMarkup}

class ToSearchableStringConverterTest extends munit.ScalaCheckSuite {

  test("Implicit ToSearchableStringConverter correctly converts simple TEXT value") {
    val sut = implicitly[ToSearchableStringConverter[Text]]
    val actual = sut.toPlainString(Text("hello world"))
    assertEquals(actual, "hello world")
  }

  test("Implicit ToSearchableStringConverter correctly converts simple HEADLINE TITLE value") {
    val sut = implicitly[ToSearchableStringConverter[Headline.Title]]
    val actual = sut.toPlainString(Headline.Title(List(Text("hello world"))))
    assertEquals(actual, "hello world")
  }

  test("Implicit ToSearchableStringConverter correctly converts nested TEXT MARKUP value") {
    val sut = implicitly[ToSearchableStringConverter[TextMarkup]]
    val actual = sut.toPlainString(
      TextMarkup(
        "",
        Marker.Bold,
        List(
          Text("hello"),
          TextMarkup.underline(" ", "underline"),
          Text(" world")
        )
      )
    )
    assertEquals(actual, "hello underline world")
  }
}
