package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._

/* For use in the If-Match, If-Range, and If-None-Match Http Headers */

sealed trait EntityTag {

  def tags: List[String]
  def value = tags.mkString(", ")
  override def toString = value
}

object EntityTags extends RegexParsers {

  private def parser = (
    "*" ^^^ Some(Star) |
    repsep(regex("\"[^,]+\"".r), regex("""[ ]*,[ ]*""".r))  ^^ {case values => values match {
      case x :: xs => Some(CustomEntityTags(values))
      case Nil => None
    }}
  )

  /* Should be an array of EntityTags */
  def parseEntityTags(inString: String) = parser(new CharSequenceReader(inString.toLowerCase)) match {
    case Success(result, _) => result

    case Failure(msg, _) => error("The EntityTags " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case object Star extends EntityTag {
    override def tags =  List("*")
  }

  sealed case class CustomEntityTags(tags: List[String]) extends EntityTag

}
