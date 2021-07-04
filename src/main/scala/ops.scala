package dev.vgerasimov.scorg

import models.elements.Paragraph
import models.objects.Text

import scala.reflect.{ ClassTag, classTag }

object ops {

  object table {
    import models.elements.TableRow.TableSep
    import models.objects.TableCell

    def $ : String => TableCell = TableCell
    def sep: TableSep.type = TableSep
  }

  private def fold[A >: B, B : ClassTag](objects: Seq[A], op: (B, B) => B): List[A] = {
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

  def foldTexts[A >: Text](objects: Seq[A]): List[A] = {
    fold[A, Text](objects, _ ++ _)
  }

  def foldParagraphs[A >: Paragraph](objects: Seq[A]): List[A] = {
    for {
      element <- fold[A, Paragraph](objects, _ ++ _)
    } yield element match {
      case Paragraph(objects) => Paragraph(foldTexts(objects))
      case _                  => element
    }
  }
}
