package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
/* For use in the X-Frame-Options Header */

sealed trait FrameOption extends ProductPrefixUnmangler {

  def name = unmangledName

  def value = name;

  override def toString = value

}

object FrameOptions {

  def parseFrameOptions(inString: String): Option[FrameOption] = {

    def outOption: Option[FrameOption]  = inString.trim.toLowerCase match {
      case "deny" => Some(DENY)
      case "sameorigin" => Some(SAMEORIGIN)
      case default => None
    }

    return outOption

  }

  case object DENY extends FrameOption

  case object SAMEORIGIN extends FrameOption

}
