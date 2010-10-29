package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex

sealed trait ConnectionToken extends ProductPrefixUnmangler {
  def value = unmangledName 
  override def toString = value
}


object ConnectionTokens {

  def parseConnectionTokens(inString: String): Option[ConnectionToken] = {
    def ConnectionRegex = new Regex("""([a-zA-Z-])+""") 
    var outConnectionTokens: Option[ConnectionToken] = ConnectionRegex.findFirstIn(inString.trim).getOrElse("")
    match {
      case "close" => Some(close)
      case "" => None
      case default => Some(CustomConnectionToken(default))
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
