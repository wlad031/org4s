package dev.vgerasimov.org4s

import models._
import models.elements.Paragraph
import models.objects.{ Text, TextMarkup }

import scala.reflect.{ ClassTag, classTag }

object ops {

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
      def asTable: Table = Table(List(row), Nil)
    }

    implicit class TableRowCellsOps(tableRowCells: TableRowCells) {
      def | (cell: TableCell): TableRowCells = TableRowCells(tableRowCells.cells ++ List(cell))
    }

    implicit class TableCellOps(cell: TableCell) {
      def | (that: TableCell): TableRowCells = TableRowCells(List(cell, that))
      def asRow: TableRowCells = TableRowCells(List(cell))
    }

    def $ : String => TableCell = TableCell.apply
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
}
