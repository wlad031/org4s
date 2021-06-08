package dev.vgerasimov.scorg
package parsers

import models.table._

import fastparse.NoWhitespace._
import fastparse._

object table {

  def table[_ : P]: P[Table] = P(tableOrg)

  def tableOrg[_ : P]: P[Table] =
    P(tableRow ~ eol ~ (tableRow ~ eol).rep ~ (tableFormula ~ eol).rep)
      .map({
        case (firstRow, restRows, formulas) => Table(firstRow :: restRows.toList, formulas.toList)
      })

  def tableRow[_ : P]: P[TableRow] = P(s.? ~ (tableRowSep | tableRowCells))

  def tableRowSep[_ : P]: P[TableSep.type] =
    P("|-" ~ ("-" | "+" | "|").rep).map(_ => TableSep)

  def tableRowCells[_ : P]: P[TableRowCells] =
    P("|" ~ tableCell ~ ("|" ~ tableCell).rep ~ "|".?)
      .map({ case (first, rest) => TableRowCells(first :: rest.toList) })

  def tableCell[_ : P]: P[TableCell] =
    anyNotIn("\n|")
      .map(s => TableCell(s.trim))

  def tableFormula[_ : P]: P[TableFormula] =
    P(s.? ~ "#+TBLFM: " ~ anyNotNL.!)
      .map(TableFormula)
}
