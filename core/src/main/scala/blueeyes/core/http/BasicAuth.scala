package blueeyes.core.http

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import org.apache.commons.codec.binary.Base64

case class BasicAuthCredentials(username: String, password: String)

object BasicAuthCredentialsParser extends RegexParsers {
  // the funky regex below detects a base64 string literal.
  private def parser = ("""[Bb]asic""".r ~> """(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?""".r) ^^ { credStr =>
    val decoded = new String(Base64.decodeBase64(credStr), "UTF-8")
    val (username, password) = decoded.splitAt(decoded.indexOf(':'))
    BasicAuthCredentials(username, password.tail)
  }

  def parse(s: String) = parseAll(parser, s) match {
    case Success(result, _) => Some(result)
    case _ => None
  }
}
// vim: set ts=4 sw=4 et:
