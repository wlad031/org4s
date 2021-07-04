package dev.vgerasimov.scorg

object context {

  case class OrgContext(
    headlineComment: String,
    todoKeywords: (Seq[String], Seq[String]),
    archiveTag: String,
    linkTypes: Seq[String],
    inlineTaskMinLevel: Int,
    elementDocumentProperties: Seq[String]
  ) {
    require(inlineTaskMinLevel > 1, "inlineTaskMinLevel must be greater than 1")
  }

  object OrgContext {
    object default {
      val headlineComment: String = "COMMENT"
      val todoKeywords: (Seq[String], Seq[String]) = (Seq("TODO"), Seq("DONE"))
      val archiveTag: String = "ARCHIVE"
      val linkTypes: Seq[String] = Seq("https", "http", "file")
      val inlineTaskMinLevel: Int = 15
      val elementDocumentProperties: Seq[String] = Seq("AUTHOR", "DATE", "TITLE")
    }

    implicit val defaultCtx: OrgContext = OrgContext(
      headlineComment = default.headlineComment,
      todoKeywords = default.todoKeywords,
      archiveTag = default.archiveTag,
      linkTypes = default.linkTypes,
      inlineTaskMinLevel = default.inlineTaskMinLevel,
      elementDocumentProperties = default.elementDocumentProperties
    )
  }
}
