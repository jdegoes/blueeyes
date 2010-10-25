package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler

/* For use in TE HTTP Header */

sealed trait TCoding extends ProductPrefixUnmangler {
  def coding: String = unmangledName
  def value: String = coding
  override def toString = value
}

object TCodings {

  def parseTCodings(inString: String): Array[TCoding] = {
    def tcodings: Array[TCoding] = inString.toLowerCase.trim.split(",").map(_.trim).map(("""[a-z]+""").r.findFirstIn(_).getOrElse(""))
      .map( _ match {
        case "trailers" => trailers
        case "deflate" => deflate
        case any => CustomTCoding(any)
    })
    return tcodings
  }

  case object trailers extends TCoding
  case object deflate extends TCoding

  sealed case class CustomTCoding(inCoding: String) extends TCoding {
    override def coding = inCoding
  }
}

