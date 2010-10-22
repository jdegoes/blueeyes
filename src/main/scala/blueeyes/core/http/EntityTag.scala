package blueeyes.core.http

import scala.util.matching.Regex

/* For use in the If-Match, If-Range, and If-None-Match Http Headers */

sealed trait EntityTag {

  def tags: List[String]
  def value = tags.mkString(",")
  override def toString = value
}

object EntityTags {

  def parseEntityTags(inString: String): EntityTag = {
    def EntityTagRegex = new Regex("""(\"([a-z]\d)+\")(, \"([a-z]\d)+\")*|\*""")
    def outTags: EntityTag = EntityTagRegex.findFirstIn(inString.trim.toLowerCase).getOrElse("").split(", ") match {
      case Array("*") => Star
      case Array("") => NullTag
      case arr => CustomEntityTags(arr: _*)
    }
    return outTags
  }

  case object Star extends EntityTag {
    override def tags =  "*" :: Nil
  }

  sealed case class CustomEntityTags(inTags: String*) extends EntityTag {
    var outTags: List[String] = Nil
    inTags.map("\"" + _ +"\"" :: outTags)
    override def tags = outTags
  }

  case object NullTag extends EntityTag {
    override def tags = Nil 
  }

}
