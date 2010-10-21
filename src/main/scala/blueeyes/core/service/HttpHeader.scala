package blueeyes.core.service

sealed trait HttpHeader extends Product2[String, String] { self =>
  def _1 = productPrefix
  def _2 = value
  
  def name: String = _1
  
  def value: String
  
  def header = _1 + ": " + _2
  
  def canEqual(any: Any) = any match {
    case header: HttpHeader => true
    case _ => false
  }
  
  override def productPrefix = self.getClass.getSimpleName
  
  override def toString = header
  
  override def equals(any: Any) = any match {
    case that: HttpHeader => (self._1.toLowerCase == that._1.toLowerCase) && 
                             (self._2.toLowerCase == that._2.toLowerCase)
    case _ => false
  }
}

object HttpHeaders {

  /************ Requests ************/

  class Accept(val mimeTypes: MimeType*) extends HttpHeader {
    def value = mimeTypes.map(_.value).mkString(",")
  }
  object Accept {
    def apply(mimeTypes: MimeType*): Accept = new Accept(mimeTypes :_*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept") Some(MimeTypes.parseMimeTypes(keyValue._2)) else None
  }
  
  class `Accept-Charset`(val value: String) extends HttpHeader 
  object `Accept-Charset` {
    def apply(charSets: CharSet*) = new `Accept-Charset`(charSets.map(_.value).mkString(";"))
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-charset") Some(keyValue._2) else None
  }

  class `Accept-Encoding`(val value: String) extends HttpHeader 
  object `Accept-Encoding` {
    def apply(encodings: Encoding*) = new `Accept-Encoding`(encodings.map(_.value).mkString(","))
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-encoding") Some(keyValue._2) else None
  }

  class `Accept-Language`(val value: String) extends HttpHeader 
  object `Accept-Language` {
    def apply(languageRange: LanguageRange*) = new `Accept-Language`(languageRange.map(_.value).mkString(","));
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-language") Some(keyValue._2) else None
  }

  class `Accept-Ranges`(val value: String) extends HttpHeader 
  object `Accept-Ranges` {
    def apply(rangeUnit: RangeUnit) = new `Accept-Ranges`(rangeUnit.value);
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-ranges") Some(keyValue._2) else None
  }

  class Authorization(val value: String) extends HttpHeader 
  object Authorization {
    def appply(credentials: String) = new Authorization(credentials) // can we do better here?
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "authorization") Some(keyValue._2) else None
  }

  class Connection(val value: String) extends HttpHeader 
  object Connection {
    def apply(connectionToken: ConnectionToken) = new Connection(connectionToken.value)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "connection") Some(keyValue._2) else None
  }

  class Cookie(val value: String) extends HttpHeader {
  }
  object Cookie {
    def apply(cookie: HttpCookie) = new Cookie(cookie.toString)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cookie") Some(keyValue._2) else None
  }

  class `Content-Length`(val length: Long) extends HttpHeader {
    def value = length.toString
  }
  object `Content-Length` {
    def apply(length: Long): `Content-Length` = new `Content-Length`(length)
    
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-length") Some(keyValue._2.toLong) else None
  }

  class `Content-Type`(val value: String) extends HttpHeader 
  object `Content-Type` {
    def apply(mimeTypes: MimeType*) = new `Content-Type`(mimeTypes.map(_.value).mkString(";"))
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-type") Some(keyValue._2) else None
  }

  class Date(val value: String) extends HttpHeader {
  }
  object Date {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "date") Some(keyValue._2) else None
  }

  class Expect(val value: String) extends HttpHeader {
  }
  object Expect {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expect") Some(keyValue._2) else None
  }

  class From(val value: String) extends HttpHeader {
  }
  object From {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "from") Some(keyValue._2) else None
  }

  class Host(val value: String) extends HttpHeader {
  }
  object Host {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "host") Some(keyValue._2) else None
  }

  class `If-Match`(val value: String) extends HttpHeader {
  }
  object `If-Match` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-match") Some(keyValue._2) else None
  }

  class `If-Modified-Since`(val value: String) extends HttpHeader {
  }
  object `If-Modified-Since` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-modified-since") Some(keyValue._2) else None
  }

  class `If-None-Match`(val value: String) extends HttpHeader {
  }
  object `If-None-Match` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-none-match") Some(keyValue._2) else None
  }

  class `If-Range`(val value: String) extends HttpHeader {
  }
  object `If-Range` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-range") Some(keyValue._2) else None
  }

  class `If-Unmodified-Since`(val value: String) extends HttpHeader {
  }
  object `If-Unmodified-Since` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-unmodified-since") Some(keyValue._2) else None
  }

  class `Max-Forwards`(val value: String) extends HttpHeader {
  }
  object `Max-Forwards` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "max-forwards") Some(keyValue._2) else None
  }

  class Pragma(val value: String) extends HttpHeader {
  }
  object Pragma {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "pragma") Some(keyValue._2) else None
  }

  class `Proxy-Authorization`(val value: String) extends HttpHeader {
  }
  object `Proxy-Authorization` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authorization") Some(keyValue._2) else None
  }

  class Range(val value: String) extends HttpHeader {
  }
  object Range {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "range") Some(keyValue._2) else None
  }

  class Referer(val value: String) extends HttpHeader {
  }
  object Referer {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "referer") Some(keyValue._2) else None
  }

  class TE(val value: String) extends HttpHeader {
  }
  object TE {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "te") Some(keyValue._2) else None
  }

  class Upgrade(val value: String) extends HttpHeader {
  }
  object Upgrade {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "upgrade") Some(keyValue._2) else None
  }

  class `User-Agent`(val value: String) extends HttpHeader {
  }
  object `User-Agent` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "user-agent") Some(keyValue._2) else None
  }

  class Via(val value: String) extends HttpHeader {
  }
  object Via {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "via") Some(keyValue._2) else None
  }

  class Warning(val value: String) extends HttpHeader {
  }
  object Warning {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "warning") Some(keyValue._2) else None
  }

  /*********** Responses ************/

  class Age(val value: String) extends HttpHeader 
  object Age {
    def apply(age: Int) = new Age(age.toString)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "age") Some(keyValue._2) else None
  }

  class Allow(val value: String) extends HttpHeader 
  object Allow {
    def apply(methods: HttpMethod*) = new Allow(methods.map(_.value).mkString(","))
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "allow") Some(keyValue._2) else None
  }

  class `Cache-Control`(val value: String) extends HttpHeader {
  }
  object `Cache-Control` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cache-control") Some(keyValue._2) else None
  }

  class `Content-Encoding`(val value: String) extends HttpHeader {
  }
  object `Content-Encoding` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-encoding") Some(keyValue._2) else None
  }

  class `Content-Language`(val value: String) extends HttpHeader {
  }
  object `Content-Language` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-language") Some(keyValue._2) else None
  }

  class `Content-Location`(val value: String) extends HttpHeader {
  }
  object `Content-Location` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-location") Some(keyValue._2) else None
  }

  class `Content-Disposition`(val value: String) extends HttpHeader {
  }
  object `Content-Disposition` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-disposition") Some(keyValue._2) else None
  }

  class `Content-MD5`(val value: String) extends HttpHeader {
  }
  object `Content-MD5` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-md5") Some(keyValue._2) else None
  }

  class `Content-Range`(val value: String) extends HttpHeader {
  }
  object `Content-Range` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-range") Some(keyValue._2) else None
  }

  class ETag(val value: String) extends HttpHeader {
  }
  object ETag {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "etag") Some(keyValue._2) else None
  }

  class Expires(val value: String) extends HttpHeader {
  }
  object Expires {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expires") Some(keyValue._2) else None
  }

  class `Last-Modified`(val value: String) extends HttpHeader {
  }
  object `Last-Modified` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "last-modified") Some(keyValue._2) else None
  }

  class Location(val value: String) extends HttpHeader {
  }
  object Location {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "location") Some(keyValue._2) else None
  }

  class `Proxy-Authenticate`(val value: String) extends HttpHeader {
  }
  object `Proxy-Authenticate` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authenticate") Some(keyValue._2) else None
  }

  class Refresh(val value: String) extends HttpHeader {
  }
  object Refresh {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "refresh") Some(keyValue._2) else None
  }

  class `Retry-After`(val value: String) extends HttpHeader {
  }
  object `Retry-After` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "retry-after") Some(keyValue._2) else None
  }

  class Server(val value: String) extends HttpHeader {
  }
  object Server {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "server") Some(keyValue._2) else None
  }

  class `Set-Cookie`(val value: String) extends HttpHeader {
  }
  object `Set-Cookie` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "set-cookie") Some(keyValue._2) else None
  }

  class Trailer(val value: String) extends HttpHeader {
  }
  object Trailer {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "trailer") Some(keyValue._2) else None
  }

  class `Transfer-Encoding`(val value: String) extends HttpHeader {
  }
  object `Transfer-Encoding` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "transfer-encoding") Some(keyValue._2) else None
  }

  class Vary(val value: String) extends HttpHeader {
  }
  object Vary {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "vary") Some(keyValue._2) else None
  }

  class `WWW-Authenticate`(val value: String) extends HttpHeader {
  }
  object `WWW-Authenticate` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "www-authenticate") Some(keyValue._2) else None
  }

  class `X-Frame-Options`(val value: String) extends HttpHeader {
  }
  object `X-Frame-Options` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-frame-options") Some(keyValue._2) else None
  }

  class `X-XSS-Protection`(val value: String) extends HttpHeader {
  }
  object `X-XSS-Protection` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-xss-protection") Some(keyValue._2) else None
  }

  class `X-Content-Type-Options`(val value: String) extends HttpHeader {
  }
  object `X-Content-Type-Options` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-content-type-options") Some(keyValue._2) else None
  }

  class `X-Requested-With`(val value: String) extends HttpHeader {
  }
  object `X-Requested-With` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-requested-with") Some(keyValue._2) else None
  }

  class `X-Forwarded-For`(val value: String) extends HttpHeader {
  }
  object `X-Forwarded-For` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-for") Some(keyValue._2) else None
  }

  class `X-Forwarded-Proto`(val value: String) extends HttpHeader {
  }
  object `X-Forwarded-Proto` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-proto") Some(keyValue._2) else None
  }

  class `X-Powered-By`(val value: String) extends HttpHeader {
  }
  object `X-Powered-By` {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-powered-by") Some(keyValue._2) else None
  }  
  
  class CustomHeader(override val name: String, val value: String) extends HttpHeader {
  }
}

trait HttpHeaderImplicits {
  import HttpHeaders._
  
  implicit def httpHeader2Tuple(httpHeader: HttpHeader) = (httpHeader._1, httpHeader._2)
  
  implicit def tuple2HttpHeader(keyValue: (String, String)): HttpHeader = keyValue match {
    case Accept(value) => new Accept(value: _*)
    case `Accept-Charset`(value) => new `Accept-Charset`(value)
    case `Accept-Encoding`(value) => new `Accept-Encoding`(value)
    case `Accept-Language`(value) => new `Accept-Language`(value)
    case `Accept-Ranges`(value) => new `Accept-Ranges`(value)
    case Authorization(value) => new Authorization(value)
    case Connection(value) => new Connection(value)
    case Cookie(value) => new Cookie(value)
    case `Content-Length`(value) => new `Content-Length`(value)
    case `Content-Type`(value) => new `Content-Type`(value)
    case Date(value) => new Date(value)
    case Expect(value) => new Expect(value)
    case From(value) => new From(value)
    case Host(value) => new Host(value)
    case `If-Match`(value) => new `If-Match`(value)
    case `If-Modified-Since`(value) => new `If-Modified-Since`(value)
    case `If-None-Match`(value) => new `If-None-Match`(value)
    case `If-Range`(value) => new `If-Range`(value)
    case `If-Unmodified-Since`(value) => new `If-Unmodified-Since`(value)
    case `Max-Forwards`(value) => new `Max-Forwards`(value)
    case Pragma(value) => new Pragma(value)
    case `Proxy-Authorization`(value) => new `Proxy-Authorization`(value)
    case Range(value) => new Range(value)
    case Referer(value) => new Referer(value)
    case TE(value) => new TE(value)
    case Upgrade(value) => new Upgrade(value)
    case `User-Agent`(value) => new `User-Agent`(value)
    case Via(value) => new Via(value)
    case Warning(value) => new Warning(value)
    case Age(value) => new Age(value)
    case Allow(value) => new Allow(value)
    case `Cache-Control`(value) => new `Cache-Control`(value)
    case `Content-Encoding`(value) => new `Content-Encoding`(value)
    case `Content-Language`(value) => new `Content-Language`(value)
    case `Content-Location`(value) => new `Content-Location`(value)
    case `Content-Disposition`(value) => new `Content-Disposition`(value)
    case `Content-MD5`(value) => new `Content-MD5`(value)
    case `Content-Range`(value) => new `Content-Range`(value)
    case ETag(value) => new ETag(value)
    case Expires(value) => new Expires(value)
    case `Last-Modified`(value) => new `Last-Modified`(value)
    case Location(value) => new Location(value)
    case `Proxy-Authenticate`(value) => new `Proxy-Authenticate`(value)
    case Refresh(value) => new Refresh(value)
    case `Retry-After`(value) => new `Retry-After`(value)
    case Server(value) => new Server(value)
    case `Set-Cookie`(value) => new `Set-Cookie`(value)
    case Trailer(value) => new Trailer(value)
    case `Transfer-Encoding`(value) => new `Transfer-Encoding`(value)
    case Vary(value) => new Vary(value)
    case `WWW-Authenticate`(value) => new `WWW-Authenticate`(value)
    case `X-Frame-Options`(value) => new `X-Frame-Options`(value)
    case `X-XSS-Protection`(value) => new `X-XSS-Protection`(value)
    case `X-Content-Type-Options`(value) => new `X-Content-Type-Options`(value)
    case `X-Requested-With`(value) => new `X-Requested-With`(value)
    case `X-Forwarded-For`(value) => new `X-Forwarded-For`(value)
    case `X-Forwarded-Proto`(value) => new `X-Forwarded-Proto`(value)
    case `X-Powered-By`(value) => new `X-Powered-By`(value)
    case (name, value) => new CustomHeader(name, value)
  }
}
object HttpHeaderImplicits extends HttpHeaderImplicits
