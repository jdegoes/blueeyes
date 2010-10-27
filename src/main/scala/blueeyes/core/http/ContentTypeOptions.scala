package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler

/* For use in the X-Content-Type-Options Header */

sealed trait ContentTypeOption extends ProductPrefixUnmangler {

  def name = unmangledName

  def value = name;

  override def toString = value

}

object ContentTypeOptions {

  def parseContentTypeOptions(inString: String): Option[ContentTypeOption] = {

    def outOption: Option[ContentTypeOption]  = inString.trim.toLowerCase match {
      case "nosniff" => Some(nosniff)
      case default => None
    }

    return outOption

  }

  case object nosniff extends ContentTypeOption

}
