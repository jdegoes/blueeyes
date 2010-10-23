package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex

sealed trait ConnectionToken extends ProductPrefixUnmangler {
  def value = unmangledName 

  override def toString = value

}

object ConnectionTokens {

  def parseConnectionTokens(inString: String): ConnectionToken = {
    def ConnectionRegex = new Regex("""([a-z]\-)+""") 
    var outConnectionTokens: ConnectionToken = ConnectionRegex.findFirstIn(inString.trim).getOrElse("") match {
      case "close" => close
      case "" => error("Shouldn't be able to create empty connection token")
      case _ => new CustomConnectionToken(inString)
    }
    return outConnectionTokens
  }

  case object close extends ConnectionToken
  //case object `Keep-Alive` extends ConnectionToken //Depreciated in HTTP 1.1
  //case object `Persist` extends ConnectionToken  //Depreciated in HTTP 1.1
  sealed case class CustomConnectionToken(override val value: String) extends ConnectionToken {
    override def toString = value
  }

}
