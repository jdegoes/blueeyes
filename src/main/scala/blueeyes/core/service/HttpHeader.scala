package blueeyes.core.service

sealed trait HttpHeader extends Product2[String, String] { self =>
  lazy val _1 = {
    def toDashes(s: List[Char]): List[Char] = s match {
      case x1 :: x2 :: xs if (x1.isLower && x2.isUpper) => x1 :: '-' :: x2 :: toDashes(xs)
      case x :: xs => x :: toDashes(xs)
      case xs => xs
    }
    
    toDashes(productPrefix.toList).mkString("")
  }
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
  class Accept(val value: String) extends HttpHeader {
  }
  object Accept {
    def apply(mimeTypes: MimeType*) = new Accept(mimeTypes.map(_.value).mkString(";"))
    
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept") Some(keyValue._2) else None
  }
  
  // import MimeTypes._
  //
  // val myAcceptHeader = Accept(image/gif, image/png, image/jpeg)

  class AcceptCharset(val value: String) extends HttpHeader {
  }
  object AcceptCharset {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-charset") Some(keyValue._2) else None
  }

  class AcceptEncoding(val value: String) extends HttpHeader {
  }
  object AcceptEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-encoding") Some(keyValue._2) else None
  }

  class AcceptLanguage(val value: String) extends HttpHeader {
  }
  object AcceptLanguage {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-language") Some(keyValue._2) else None
  }

  class AcceptRanges(val value: String) extends HttpHeader {
  }
  object AcceptRanges {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-ranges") Some(keyValue._2) else None
  }

  class Authorization(val value: String) extends HttpHeader {
  }
  object Authorization {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "authorization") Some(keyValue._2) else None
  }

  class Connection(val value: String) extends HttpHeader {
  }
  object Connection {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "connection") Some(keyValue._2) else None
  }

  class Cookie(val value: String) extends HttpHeader {
  }
  object Cookie {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cookie") Some(keyValue._2) else None
  }

  class ContentLength(val value: String) extends HttpHeader {
  }
  object ContentLength {
    def apply(length: Long): ContentLength = new ContentLength(length.toString)
    
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-length") Some(keyValue._2) else None
  }

  class ContentType(val value: String) extends HttpHeader {
  }
  object ContentType {
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

  class IfMatch(val value: String) extends HttpHeader {
  }
  object IfMatch {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-match") Some(keyValue._2) else None
  }

  class IfModifiedSince(val value: String) extends HttpHeader {
  }
  object IfModifiedSince {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-modified-since") Some(keyValue._2) else None
  }

  class IfNoneMatch(val value: String) extends HttpHeader {
  }
  object IfNoneMatch {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-none-match") Some(keyValue._2) else None
  }

  class IfRange(val value: String) extends HttpHeader {
  }
  object IfRange {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-range") Some(keyValue._2) else None
  }

  class IfUnmodifiedSince(val value: String) extends HttpHeader {
  }
  object IfUnmodifiedSince {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-unmodified-since") Some(keyValue._2) else None
  }

  class MaxForwards(val value: String) extends HttpHeader {
  }
  object MaxForwards {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "max-forwards") Some(keyValue._2) else None
  }

  class Pragma(val value: String) extends HttpHeader {
  }
  object Pragma {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "pragma") Some(keyValue._2) else None
  }

  class ProxyAuthorization(val value: String) extends HttpHeader {
  }
  object ProxyAuthorization {
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

  class UserAgent(val value: String) extends HttpHeader {
  }
  object UserAgent {
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

  class Age(val value: String) extends HttpHeader {
  }
  object Age {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "age") Some(keyValue._2) else None
  }

  class Allow(val value: String) extends HttpHeader {
  }
  object Allow {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "allow") Some(keyValue._2) else None
  }

  class CacheControl(val value: String) extends HttpHeader {
  }
  object CacheControl {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cache-control") Some(keyValue._2) else None
  }

  class ContentEncoding(val value: String) extends HttpHeader {
  }
  object ContentEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-encoding") Some(keyValue._2) else None
  }

  class ContentLanguage(val value: String) extends HttpHeader {
  }
  object ContentLanguage {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-language") Some(keyValue._2) else None
  }

  class ContentLocation(val value: String) extends HttpHeader {
  }
  object ContentLocation {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-location") Some(keyValue._2) else None
  }

  class ContentDisposition(val value: String) extends HttpHeader {
  }
  object ContentDisposition {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-disposition") Some(keyValue._2) else None
  }

  class ContentMD5(val value: String) extends HttpHeader {
  }
  object ContentMD5 {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-md5") Some(keyValue._2) else None
  }

  class ContentRange(val value: String) extends HttpHeader {
  }
  object ContentRange {
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

  class LastModified(val value: String) extends HttpHeader {
  }
  object LastModified {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "last-modified") Some(keyValue._2) else None
  }

  class Location(val value: String) extends HttpHeader {
  }
  object Location {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "location") Some(keyValue._2) else None
  }

  class ProxyAuthenticate(val value: String) extends HttpHeader {
  }
  object ProxyAuthenticate {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authenticate") Some(keyValue._2) else None
  }

  class Refresh(val value: String) extends HttpHeader {
  }
  object Refresh {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "refresh") Some(keyValue._2) else None
  }

  class RetryAfter(val value: String) extends HttpHeader {
  }
  object RetryAfter {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "retry-after") Some(keyValue._2) else None
  }

  class Server(val value: String) extends HttpHeader {
  }
  object Server {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "server") Some(keyValue._2) else None
  }

  class SetCookie(val value: String) extends HttpHeader {
  }
  object SetCookie {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "set-cookie") Some(keyValue._2) else None
  }

  class Trailer(val value: String) extends HttpHeader {
  }
  object Trailer {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "trailer") Some(keyValue._2) else None
  }

  class TransferEncoding(val value: String) extends HttpHeader {
  }
  object TransferEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "transfer-encoding") Some(keyValue._2) else None
  }

  class Vary(val value: String) extends HttpHeader {
  }
  object Vary {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "vary") Some(keyValue._2) else None
  }

  class WWWAuthenticate(val value: String) extends HttpHeader {
  }
  object WWWAuthenticate {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "www-authenticate") Some(keyValue._2) else None
  }

  class XFrameOptions(val value: String) extends HttpHeader {
  }
  object XFrameOptions {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-frame-options") Some(keyValue._2) else None
  }

  class XXSSProtection(val value: String) extends HttpHeader {
  }
  object XXSSProtection {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-xss-protection") Some(keyValue._2) else None
  }

  class XContentTypeOptions(val value: String) extends HttpHeader {
  }
  object XContentTypeOptions {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-content-type-options") Some(keyValue._2) else None
  }

  class XRequestedWith(val value: String) extends HttpHeader {
  }
  object XRequestedWith {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-requested-with") Some(keyValue._2) else None
  }

  class XForwardedFor(val value: String) extends HttpHeader {
  }
  object XForwardedFor {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-for") Some(keyValue._2) else None
  }

  class XForwardedProto(val value: String) extends HttpHeader {
  }
  object XForwardedProto {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-proto") Some(keyValue._2) else None
  }

  class XPoweredBy(val value: String) extends HttpHeader {
  }
  object XPoweredBy {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-powered-by") Some(keyValue._2) else None
  }  
  
  class CustomHeader(override val name: String, val value: String) extends HttpHeader {
  }
}

trait HttpHeaderImplicits {
  import HttpHeaders._
  
  implicit def httpHeader2Tuple(httpHeader: HttpHeader) = (httpHeader._1, httpHeader._2)
  
  implicit def tuple2HttpHeader(keyValue: (String, String)): HttpHeader = keyValue match {
    case Accept(value) => new Accept(value)
    case AcceptCharset(value) => new AcceptCharset(value)
    case AcceptEncoding(value) => new AcceptEncoding(value)
    case AcceptLanguage(value) => new AcceptLanguage(value)
    case AcceptRanges(value) => new AcceptRanges(value)
    case Authorization(value) => new Authorization(value)
    case Connection(value) => new Connection(value)
    case Cookie(value) => new Cookie(value)
    case ContentLength(value) => new ContentLength(value)
    case ContentType(value) => new ContentType(value)
    case Date(value) => new Date(value)
    case Expect(value) => new Expect(value)
    case From(value) => new From(value)
    case Host(value) => new Host(value)
    case IfMatch(value) => new IfMatch(value)
    case IfModifiedSince(value) => new IfModifiedSince(value)
    case IfNoneMatch(value) => new IfNoneMatch(value)
    case IfRange(value) => new IfRange(value)
    case IfUnmodifiedSince(value) => new IfUnmodifiedSince(value)
    case MaxForwards(value) => new MaxForwards(value)
    case Pragma(value) => new Pragma(value)
    case ProxyAuthorization(value) => new ProxyAuthorization(value)
    case Range(value) => new Range(value)
    case Referer(value) => new Referer(value)
    case TE(value) => new TE(value)
    case Upgrade(value) => new Upgrade(value)
    case UserAgent(value) => new UserAgent(value)
    case Via(value) => new Via(value)
    case Warning(value) => new Warning(value)
    case Age(value) => new Age(value)
    case Allow(value) => new Allow(value)
    case CacheControl(value) => new CacheControl(value)
    case ContentEncoding(value) => new ContentEncoding(value)
    case ContentLanguage(value) => new ContentLanguage(value)
    case ContentLocation(value) => new ContentLocation(value)
    case ContentDisposition(value) => new ContentDisposition(value)
    case ContentMD5(value) => new ContentMD5(value)
    case ContentRange(value) => new ContentRange(value)
    case ETag(value) => new ETag(value)
    case Expires(value) => new Expires(value)
    case LastModified(value) => new LastModified(value)
    case Location(value) => new Location(value)
    case ProxyAuthenticate(value) => new ProxyAuthenticate(value)
    case Refresh(value) => new Refresh(value)
    case RetryAfter(value) => new RetryAfter(value)
    case Server(value) => new Server(value)
    case SetCookie(value) => new SetCookie(value)
    case Trailer(value) => new Trailer(value)
    case TransferEncoding(value) => new TransferEncoding(value)
    case Vary(value) => new Vary(value)
    case WWWAuthenticate(value) => new WWWAuthenticate(value)
    case XFrameOptions(value) => new XFrameOptions(value)
    case XXSSProtection(value) => new XXSSProtection(value)
    case XContentTypeOptions(value) => new XContentTypeOptions(value)
    case XRequestedWith(value) => new XRequestedWith(value)
    case XForwardedFor(value) => new XForwardedFor(value)
    case XForwardedProto(value) => new XForwardedProto(value)
    case XPoweredBy(value) => new XPoweredBy(value)
    case (name, value) => new CustomHeader(name, value)
  }
}
object HttpHeaderImplicits extends HttpHeaderImplicits