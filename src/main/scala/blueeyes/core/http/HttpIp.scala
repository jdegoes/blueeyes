package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.parsing.combinator._
import scala.util.parsing.input._

import java.net.InetAddress

/* For use in the X-Forwarded-For Header */

sealed trait HttpIp {

  def ip: InetAddress

  def value: String = ip.getHostAddress

  override def toString = value

}

object HttpIps extends RegexParsers{

  private def elementParser = regex("""([\d]{1,3}\.){3}[\d]{1,3}""".r) ^^ {case v => CustomIP(InetAddress.getByName(v))}

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r))

  def parseSingleIp(inString: String): Option[HttpIp] = parseHttpIps(inString).headOption

  def parseHttpIps(inString: String): List[HttpIp] = parser(new CharSequenceReader(inString)) match {
    case Success(result, _) => result

    case Failure(msg, _) => sys.error("The HttpIps " + inString + " has a syntax error: " + msg)

    case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
  }

  case class CustomIP(ip: InetAddress) extends HttpIp

}

