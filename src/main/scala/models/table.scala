package dev.vgerasimov.scorg
package models

/** Contains data classes representing Org tables. */
object table {

  case class Table(rows: List[TableRow], formulas: List[TableFormula]) {
    def + (that: TableRow): Table = Table(rows ++ List(that), formulas)
    def + (formula: TableFormula): Table = Table(rows, formulas ++ List(formula))
  }

  sealed trait TableRow {
    def + (that: TableRow): Table = Table(List(this, that), Nil)
    def + (formula: TableFormula): Table = Table(List(this), List(formula))

    def asTable: Table = Table(List(this), Nil)
  }
  case object TableSep extends TableRow
  case class TableRowCells(cells: List[TableCell]) extends TableRow {
    def | (cell: TableCell): TableRowCells = TableRowCells(cells ++ List(cell))
  }

  case class TableCell(value: String) {
    def | (that: TableCell): TableRowCells = TableRowCells(List(this, that))
  }

  case class TableFormula(value: String)

  def $ : String => TableCell = TableCell
  def sep: TableSep.type = TableSep
}
