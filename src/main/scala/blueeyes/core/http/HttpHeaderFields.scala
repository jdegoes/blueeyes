package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait HttpHeaderField extends ProductPrefixUnmangler {
  
  def field: String = unmangledName

  def value = field

  override def toString = field

}

/* Careful with namespace pollution - these are precisely the same names as
 * those in HttpHeader */
object HttpHeaderFields extends RegexParsers{

  def elementParser: Parser[HttpHeaderField] = (
    "accept" ^^^ Accept  |
    "accept-charset" ^^^ `Accept-Charset`  |
    "accept-encoding" ^^^ `Accept-Encoding`  |
    "accept-language" ^^^ `Accept-Language`  |
    "accept-ranges" ^^^ `Accept-Ranges`  |
    "authorization" ^^^ Authorization  |
    "connection" ^^^ Connection  |
    "cookie" ^^^ Cookie  |
    "content-length" ^^^ `Content-Length` | // Trailer cannot include
    "content-type" ^^^ `Content-Type`  |
    "date" ^^^ Date  |
    "expect" ^^^ Expect  |
    "from" ^^^ From  |
    "host" ^^^ Host  |
    "if-match" ^^^ `If-Match`  |
    "if-modified-since" ^^^ `If-Modified-Since`  |
    "if-none-match" ^^^ `If-None-Match`  |
    "if-range" ^^^ `If-Range`  |
    "if-unmodified-since" ^^^ `If-Unmodified-Since`  |
    "max-forwards" ^^^ `Max-Forwards`  |
    "pragma" ^^^ Pragma  |
    "proxy-authorization" ^^^ `Proxy-Authorization`  |
    "range" ^^^ Range  |
    "referer" ^^^ Referer  |
    "te" ^^^ TE  |
    "upgrade" ^^^ Upgrade  |
    "user-agent" ^^^ `User-Agent`  |
    "via" ^^^ Via  |
    "warning" ^^^ Warning  |
    "age" ^^^ Age  | /* Response */
    "allow" ^^^ Allow  |
    "cache-control" ^^^ `Cache-Control`  |
    "content-encoding" ^^^ `Content-Encoding`  |
    "content-language" ^^^ `Content-Language`  |
    "content-location" ^^^ `Content-Location`  |
    "content-disposition" ^^^ `Content-Disposition`  |
    "content-md5" ^^^ `Content-MD5`  |
    "content-range" ^^^ `Content-Range`  |
    "etag" ^^^ ETag  |
    "expires" ^^^ Expires  |
    "last-modified" ^^^ `Last-Modified`  |
    "location" ^^^ Location  |
    "proxy-authenticate" ^^^ `Proxy-Authenticate`  |
    "refresh" ^^^ Refresh  |
    "retry-after" ^^^ `Retry-After`  |
    "server" ^^^ Server  |
    "set-cookie" ^^^ `Set-Cookie`  |
    "trailer" ^^^ Trailer |                         // Trailer cannot include
    "transfer-encoding" ^^^ `Transfer-Encoding` |  // Trailer cannot include
    "vary" ^^^ Vary  |
    "www-authenticate" ^^^ `WWW-Authenticate`  |
    "x-frame-option" ^^^ `X-Frame-Options`  | /* Extra Headers */
    "x-xss-protection" ^^^ `X-XSS-Protection`  |
    "x-content-type-options" ^^^ `X-Content-Type-Options`  |
    "x-requested-with" ^^^ `X-Requested-With`  |
    "x-forwarded-for" ^^^ `X-Forwarded-For`  |
    "x-forwarded-proto" ^^^ `X-Forwarded-Proto`  |
    "x-powered-by" ^^^ `X-Powered-By` |
    regex("[^,]+".r) ^^^ NullHeader
  )

  private def parser = repsep(elementParser, regex("""[ ]*,[ ]*""".r))

  def parseHttpHeaderFields(inString: String, parseType: String): List[HttpHeaderField] = {
    def outFields = parser(new CharSequenceReader(inString.toLowerCase)) match {
      case Success(result, _) => result

      case Failure(msg, _) => sys.error("The HttpHeaderFields " + inString + " has a syntax error: " + msg)

      case Error(msg, _) => sys.error("There was an error parsing \"" + inString + "\": " + msg)
    }

    def notInTrailer(x: HttpHeaderField): Boolean = {
      x match {
        case Trailer => true
        case `Transfer-Encoding` => true
        case `Content-Length` => true
        case NullHeader => true
        case _ => false
      }
    }

    val ss = outFields
    if (parseType == "trailer")
      return outFields.filterNot(notInTrailer(_))
    else
      return outFields.filterNot(_ match {
        case NullHeader => true
        case _ => false })
  }

  /** Requests **/
  case object Accept extends HttpHeaderField
  case object `Accept-Charset` extends HttpHeaderField
  case object `Accept-Encoding` extends HttpHeaderField
  case object `Accept-Language` extends HttpHeaderField
  case object `Accept-Ranges` extends HttpHeaderField
  case object Authorization extends HttpHeaderField
  case object Connection extends HttpHeaderField
  case object Cookie extends HttpHeaderField
  case object `Content-Length` extends HttpHeaderField
  case object `Content-Type` extends HttpHeaderField
  case object Date extends HttpHeaderField
  case object Expect extends HttpHeaderField
  case object From extends HttpHeaderField
  case object Host extends HttpHeaderField
  case object `If-Match` extends HttpHeaderField
  case object `If-Modified-Since` extends HttpHeaderField
  case object `If-None-Match` extends HttpHeaderField
  case object `If-Range` extends HttpHeaderField
  case object `If-Unmodified-Since` extends HttpHeaderField
  case object `Max-Forwards` extends HttpHeaderField
  case object Pragma extends HttpHeaderField
  case object `Proxy-Authorization` extends HttpHeaderField
  case object Range extends HttpHeaderField
  case object Referer extends HttpHeaderField
  case object TE extends HttpHeaderField
  case object Upgrade extends HttpHeaderField
  case object `User-Agent` extends HttpHeaderField
  case object Via extends HttpHeaderField
  case object Warning extends HttpHeaderField

  /* Response */
  case object Age extends HttpHeaderField
  case object Allow extends HttpHeaderField
  case object `Cache-Control` extends HttpHeaderField
  case object `Content-Encoding` extends HttpHeaderField
  case object `Content-Language` extends HttpHeaderField
  case object `Content-Location` extends HttpHeaderField
  case object `Content-Disposition` extends HttpHeaderField
  case object `Content-MD5` extends HttpHeaderField
  case object `Content-Range` extends HttpHeaderField
  case object ETag extends HttpHeaderField
  case object Expires extends HttpHeaderField
  case object `Last-Modified` extends HttpHeaderField
  case object Location extends HttpHeaderField
  case object `Proxy-Authenticate` extends HttpHeaderField
  case object Refresh extends HttpHeaderField
  case object `Retry-After` extends HttpHeaderField
  case object Server extends HttpHeaderField
  case object `Set-Cookie` extends HttpHeaderField
  case object Trailer extends HttpHeaderField
  case object `Transfer-Encoding` extends HttpHeaderField
  case object Vary extends HttpHeaderField
  case object `WWW-Authenticate` extends HttpHeaderField

  /* Extra Headers */
  case object `X-Frame-Options` extends HttpHeaderField
  case object `X-XSS-Protection` extends HttpHeaderField
  case object `X-Content-Type-Options` extends HttpHeaderField
  case object `X-Requested-With` extends HttpHeaderField
  case object `X-Forwarded-For` extends HttpHeaderField
  case object `X-Forwarded-Proto` extends HttpHeaderField
  case object `X-Powered-By` extends HttpHeaderField

  case object NullHeader extends HttpHeaderField {
    override def field = ""
  }

}
