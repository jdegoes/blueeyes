package blueeyes.core.http

import scala.util.matching.Regex


sealed trait HttpDomain {

  def domainName: String
  def port: String
  def value = (domainName.toList ++ port.toList :: Nil).mkString(": ")
  override def toString = value
}

object HttpDomains {

  def parseHttpDomains(inString: String): HttpDomain = {
    def DomainRegex = new Regex("""([a-z\.])+(:(\d)+)?""")
    def outDomain: HttpDomain = DomainRegex.findFirstIn(inString).getOrElse("").split(":") match {
      case Array(val1, val2)    => CustomDomain(val1, val2)
      case Array(value)         => CustomDomain(value, "")
      case _                    => NullDomain()
    }
    return outDomain
  }

  case class CustomDomain(domainName: String, port: String) extends HttpDomain

  object NullDomain {
    def apply() = new CustomDomain("","") 
  }
  
}
