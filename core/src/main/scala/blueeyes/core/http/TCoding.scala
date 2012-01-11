package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/* For use in TE HTTP Header */

sealed trait TCoding extends ProductPrefixUnmangler {
  def coding: String = unmangledName
  def value: String = coding
  override def toString = value
}

object TCodings extends RegexParsers {

  private def elementParser = (
    "trailers" ^^^ trailers |
    "deflate" ^^^ deflate |
    regex("""[a-z]+""".r) ^^ {case value => CustomTCoding(value)}
  )?

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r))

  def parseTCodings(inString: String): List[TCoding] = parser(new CharSequenceReader(inString.toLowerCase.trim)) match {
    case Success(result, _) => result.flatten

    case Failure(msg, _) => sys.error("The TCodings " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case object trailers extends TCoding
  case object deflate extends TCoding

  sealed case class CustomTCoding(inCoding: String) extends TCoding {
    override def coding = inCoding
  }
}

