package blueeyes.core.http

import scala.util.matching.Regex

/* For use in the If-Match, If-Range, and If-None-Match Http Headers */

sealed trait EntityTag {

  def tags: Array[String]
  def value = tags.mkString(", ")
  override def toString = value
}

object EntityTags {

  /* Should be an array of EntityTags */
  def parseEntityTags(inString: String): Option[EntityTag] = {
    def EntityTagRegex = new Regex("""\*|(\".*\")""")
    def outString: Option[String] = EntityTagRegex.findFirstIn(inString.trim.toLowerCase)

    def outTags: Option[EntityTag] = outString match {
      case Some("*") => Some(Star)
      case Some(any) => Some(CustomEntityTags(any.split(",").map(_.trim)))
      case None => None
    }
    return outTags
  }

  case object Star extends EntityTag {
    override def tags =  Array("*")
  }

  sealed case class CustomEntityTags(tags: Array[String]) extends EntityTag 

}
