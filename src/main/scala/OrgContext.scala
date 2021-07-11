package dev.vgerasimov.scorg

/** Configuration of the Org document. */
case class OrgContext(
  headlineComment: String,
  todoKeywords: OrgContext.TodoKeywords,
  archiveTag: String,
  linkTypes: Seq[String],
  inlineTaskMinLevel: Int,
  elementDocumentProperties: Seq[String]
) {
  require(inlineTaskMinLevel > 1, "inlineTaskMinLevel must be greater than 1")
}

object OrgContext {

  /** Contains default values for all fields of the [[OrgContext]]. */
  object default {
    val headlineComment: String = "COMMENT"
    val todoKeywords: TodoKeywords = TodoKeywords(
      List(TodoKeywords.KWSet(List("TODO"), List("DONE")))
    )
    val archiveTag: String = "ARCHIVE"
    val linkTypes: Seq[String] = Seq("https", "http", "file")
    val inlineTaskMinLevel: Int = 15
    val elementDocumentProperties: Seq[String] = Seq("AUTHOR", "DATE", "TITLE")
  }

  /** Default instance of [[OrgContext]]. */
  implicit val defaultCtx: OrgContext = OrgContext(
    headlineComment = default.headlineComment,
    todoKeywords = default.todoKeywords,
    archiveTag = default.archiveTag,
    linkTypes = default.linkTypes,
    inlineTaskMinLevel = default.inlineTaskMinLevel,
    elementDocumentProperties = default.elementDocumentProperties
  )

  /** Contains collections of valid "to-do" keywords. */
  case class TodoKeywords(sets: List[TodoKeywords.KWSet]) {
    def contains(s: String): Boolean = sets.exists(_.contains(s))
    def findSet(s: String): Option[TodoKeywords.KWSet] = sets.find(_.contains(s))
    def allValues: Seq[String] = sets.flatMap(set => set.todo ++ set.done)
    def ++ (that: TodoKeywords): TodoKeywords =
      TodoKeywords(this.sets ++ that.sets.filter(s => !this.sets.contains(s)))
  }

  object TodoKeywords {
    case class KWSet(todo: List[String], done: List[String]) {
      def contains(s: String): Boolean = todo.contains(s) || done.contains(s)
    }
  }
}
