package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scala.util.matching.Regex


sealed trait HttpHeaderField extends ProductPrefixUnmangler {
  
  def field: String = unmangledName

  def value = field

  override def toString = field

}

/* Careful with namespace pollution - these are precisely the same names as
 * those in HttpHeader */
object HttpHeaderFields {

  def parseHttpHeaderFields(inString: String, parseType: String): Option[Array[HttpHeaderField]] = {
    def outFields = inString.toLowerCase.trim.split(",").toList.map(_.trim match {
        case "accept" => Accept 
        case "accept-charset" => `Accept-Charset` 
        case "accept-encoding" => `Accept-Encoding` 
        case "accept-language" => `Accept-Language` 
        case "accept-ranges" => `Accept-Ranges` 
        case "authorization" => Authorization 
        case "connection" => Connection 
        case "cookie" => Cookie 
        case "content-length" => `Content-Length` // Trailer cannot include
        case "content-type" => `Content-Type` 
        case "date" => Date 
        case "expect" => Expect 
        case "from" => From 
        case "host" => Host 
        case "if-match" => `If-Match` 
        case "if-modified-since" => `If-Modified-Since` 
        case "if-none-match" => `If-None-Match` 
        case "if-range" => `If-Range` 
        case "if-unmodified-since" => `If-Unmodified-Since` 
        case "max-forwards" => `Max-Forwards` 
        case "pragma" => Pragma 
        case "proxy-authorization" => `Proxy-Authorization` 
        case "range" => Range 
        case "referer" => Referer 
        case "te" => TE 
        case "upgrade" => Upgrade 
        case "user-agent" => `User-Agent` 
        case "via" => Via 
        case "warning" => Warning 

        /* Response */
        case "age" => Age 
        case "allow" => Allow 
        case "cache-control" => `Cache-Control` 
        case "content-encoding" => `Content-Encoding` 
        case "content-language" => `Content-Language` 
        case "content-location" => `Content-Location` 
        case "content-disposition" => `Content-Disposition` 
        case "content-md5" => `Content-MD5` 
        case "content-range" => `Content-Range` 
        case "etag" => ETag 
        case "expires" => Expires 
        case "last-modified" => `Last-Modified` 
        case "location" => Location 
        case "proxy-authenticate" => `Proxy-Authenticate` 
        case "refresh" => Refresh 
        case "retry-after" => `Retry-After` 
        case "server" => Server 
        case "set-cookie" => `Set-Cookie` 
        case "trailer" => Trailer                         // Trailer cannot include
        case "transfer-encoding" => `Transfer-Encoding`   // Trailer cannot include
        case "vary" => Vary 
        case "www-authenticate" => `WWW-Authenticate` 

        /* Extra Headers */
        case "x-frame-option" => `X-Frame-Options` 
        case "x-xss-protection" => `X-XSS-Protection` 
        case "x-content-type-options" => `X-Content-Type-Options` 
        case "x-requested-with" => `X-Requested-With` 
        case "x-forwarded-for" => `X-Forwarded-For` 
        case "x-forwarded-proto" => `X-Forwarded-Proto` 
        case "x-powered-by" => `X-Powered-By` 
        case default => NullHeader
    })

    def notInTrailer(x: HttpHeaderField): Boolean = {
      x match {
        case Trailer => true
        case `Transfer-Encoding` => true
        case `Content-Length` => true
        case NullHeader => true
        case _ => false
      }
    }

    if (parseType == "trailer") 
      return Some(outFields.filterNot(notInTrailer(_)).toArray)
    else 
      return Some(outFields.filterNot(_ match {
        case NullHeader => true
        case _ => false }).toArray)
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
