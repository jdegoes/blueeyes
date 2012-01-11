package blueeyes.core.http
import blueeyes.util.ProductPrefixUnmangler
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/* For use in the Accept-Ranges Http Header */

sealed trait RangeUnit extends ProductPrefixUnmangler{
  def value: String = unmangledName 
  
  override def toString = value
}

object RangeUnits extends RegexParsers{

  private def parser = (
    "bytes" ^^^ bytes |
    "none" ^^^ none
  )?

  def parseRangeUnits(inString: String): Option[RangeUnit] = parser(new CharSequenceReader(inString.toLowerCase.trim)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The RangeUnits " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case object none extends RangeUnit
  case object bytes extends RangeUnit

  sealed case class CustomToken(override val value: String) extends RangeUnit
}
