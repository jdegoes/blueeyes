package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import blueeyes.util.ProductPrefixUnmangler

sealed trait ConnectionToken extends ProductPrefixUnmangler {
  def value = unmangledName 
  override def toString = value
}

object ConnectionTokens extends RegexParsers {

  private def parser = (
    "close" ^^^ close |
     regex("""([a-zA-Z-])+"""r) ^^ {case value => CustomConnectionToken(value)}
  )?

  def parseConnectionTokens(inString: String) = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The ConnectionTokens " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case object close extends ConnectionToken
  //case object `Keep-Alive` extends ConnectionToken //Depreciated in HTTP 1.1
  //case object `Persist` extends ConnectionToken  //Depreciated in HTTP 1.1
  sealed case class CustomConnectionToken(override val value: String) extends ConnectionToken {
    override def toString = value
  }

}
