package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex

/*
  Encodings for AcceptEncoding
*/

sealed trait Encoding extends ProductPrefixUnmangler {
  def value: String = unmangledName 

  override def toString = value
}

object Encodings {

  def parseEncodings(inString: String): Array[Encoding] = {
    def EncodingRegex = new Regex("""([a-z]\-\*)+""")

    var outEncodings: Array[Encoding] = inString.split(",").map(_.trim)
        .flatMap(EncodingRegex findFirstIn _)
        .map ( encoding =>  encoding match { 
            case "compress" => compress  
            case "chunked" => chunked
            case "deflate" => deflate
            case "gzip" => gzip 
            case "identity" => identity 
            case "x-compress" => `x-compress`
            case "x-zip" => `x-zip`
            case "*" => `*`
            case _ => new CustomEncoding(encoding) 
          }
        )
    return outEncodings
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
