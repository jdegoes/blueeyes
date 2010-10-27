package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler

/* For use in the X-Content-Type-Options Header */

sealed trait RequestedWith extends ProductPrefixUnmangler {

  def name = unmangledName

  def value = name;

  override def toString = value

}

object RequestedWiths {

  def parseRequestedWiths(inString: String): Option[RequestedWith] = {

    def outOption: Option[RequestedWith]  = inString.trim.toLowerCase match {
      case "XMLHttpRequest" => Some(XMLHttpRequest)
      case default => Some(CustomRequested(default))
    }

    outOption
  }

  case object XMLHttpRequest extends RequestedWith
  case class CustomRequested(inName: String) extends RequestedWith {
    override def name = inName 
  }


}
