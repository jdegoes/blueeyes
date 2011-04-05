package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.parsing.combinator._
import scala.util.parsing.input._

/*
  Encodings for AcceptEncoding
*/

sealed trait Encoding extends ProductPrefixUnmangler {
  def value: String = unmangledName 

  override def toString = value
}

object Encodings extends RegexParsers {

  private def elementParser = regex("""([a-z\-\*])+""".r) ^^ {case encoding => encoding match {
      case "compress"   => compress
      case "chunked"    => chunked
      case "deflate"    => deflate
      case "gzip"       => gzip
      case "identity"   => identity
      case "x-compress" => `x-compress`
      case "x-zip"      => `x-zip`
      case "*"          => `*`
      case _            => new CustomEncoding(encoding)
    }
  }

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r))

  def parseEncodings(inString: String) = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => error("The Encodings " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case object compress extends Encoding
  case object chunked extends Encoding
  case object deflate extends Encoding
  case object gzip extends Encoding
  case object identity extends Encoding
  case object `x-compress` extends Encoding 
  case object `x-zip` extends Encoding
  case object `*` extends Encoding
  
  sealed case class CustomEncoding(override val value: String) extends Encoding
}
