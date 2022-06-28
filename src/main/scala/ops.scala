package dev.vgerasimov.org4s

import models._
import models.elements.Paragraph
import models.objects.{ Text, TextMarkup }

import scala.reflect.{ ClassTag, classTag }

object ops {

  implicit class DocumentOps(document: Document) {

    def filterSections(f: Section => Boolean): Document = ???

    def mapSections(f: Section => Boolean): Document = ???

    def filterElements(f: Section => Boolean): Document = ???

//    private val toSearchableStringConverter =
//      implicitly[ToSearchableStringConverter[Headline.Title]]

//    private implicit val matcher: Matcher = toLowerCaseMatcher.and(bySubStringMatcher)

//    def findSectionByTitle(titles: String*)(implicit matcher: Matcher = matcher): Option[Section] = {
//      def iter(
//        titles: List[String],
//        sections: List[Section],
//        result: Option[Section]
//      ): Option[Section] = {
//        titles match {
//          case Nil => result
//          case titleToFind :: nextTitlesToFind =>
//            sections
//              .map(sec => (sec, sec.headline.title.map(toSearchableStringConverter.toPlainString)))
//              .find({
//                case (_, Some(titleContent)) =>
//                  matcher.isMatched(titleContent, titleToFind).isMatched
//                case (_, None) => false
//              })
//              .flatMap({ case (sec, _) => iter(nextTitlesToFind, sec.childSections, Some(sec)) })
//        }
//      }
//      iter(titles.toList, document.sections, None)
//    }
  }

  object table {
    import models.elements.Keyword.TableFormula
    import models.elements.TableRow
    import models.elements.TableRow.{ TableRowCells, TableSep }
    import models.greater_elements.Table
    import models.objects.TableCell

    implicit class TableOps(table: Table) {
      def + (that: elements.TableRow): Table = Table(table.rows ++ List(that), table.formulas)
      def + (formula: elements.Keyword.TableFormula): Table =
        Table(table.rows, table.formulas ++ List(formula))
    }

    implicit class TableRowOps(row: TableRow) {
      def + (that: TableRow): Table = Table(List(row, that), Nil)
      def + (formula: TableFormula): Table = Table(List(row), List(formula))
    }

    implicit class TableRowCellsOps(tableRowCells: TableRowCells) {
      def | (cell: TableCell): TableRowCells = TableRowCells(tableRowCells.cells ++ List(cell))
    }

    implicit class TableCellOps(cell: TableCell) {
      def | (that: TableCell): TableRowCells = TableRowCells(List(cell, that))
    }

    def $ : String => TableCell = TableCell
    def sep: TableSep.type = TableSep
  }

  private def fold[A >: B, B : ClassTag](objects: List[A], op: (B, B) => B): List[A] = {
    def aIsB(a: A): Boolean = classTag[B].runtimeClass.isInstance(a)
    objects
      .foldLeft[List[A]](Nil)((ls, item) =>
        item match {
          case x if aIsB(x) =>
            ls match {
              case ::(head, next) =>
                if (aIsB(head)) op(head.asInstanceOf[B], x.asInstanceOf[B]) :: next
                else x :: head :: next
              case Nil => List(x)
            }
          case x => x :: ls
        }
      )
      .reverse
  }

  private[org4s] def foldTexts[A >: Text](objects: List[A]): List[A] =
    fold[A, Text](objects, _ ++ _)

  private[org4s] def foldParagraphs[A >: Paragraph](objects: List[A]): List[A] =
    for {
      element <- fold[A, Paragraph](objects, _ ++ _)
    } yield element match {
      case Paragraph(objects) => Paragraph(foldTexts(objects))
      case _                  => element
    }



//  trait Matcher {
//    def isMatched(matchIn: String, toMatch: String): Matcher.MatchingResult
//
//    def and(that: Matcher): Matcher =
//      (matchIn: String, toMatch: String) => {
//        val first = this.isMatched(matchIn, toMatch)
//        if (!first.isMatched) Matcher.MatchingResult(isMatched = false, matchIn, toMatch)
//        else {
//          val second = that.isMatched(first.modifiedMatchId, first.modifiedToMatch)
//          Matcher.MatchingResult(
//            first.isMatched && second.isMatched,
//            second.modifiedToMatch,
//            second.modifiedMatchId
//          )
//        }
//      }
//  }
//
//  object Matcher {
//    case class MatchingResult(isMatched: Boolean, modifiedMatchId: String, modifiedToMatch: String)
//  }
//
//  val bySubStringMatcher: Matcher = (matchIn: String, toMatch: String) =>
//    Matcher.MatchingResult(matchIn.contains(toMatch), matchIn, toMatch)
//
//  val toLowerCaseMatcher: Matcher = (matchIn: String, toMatch: String) =>
//    Matcher.MatchingResult(isMatched = true, matchIn.toLowerCase(), toMatch.toLowerCase())
}
