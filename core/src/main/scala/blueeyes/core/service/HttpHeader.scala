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
  
  def productPrefix: String
  
  def name: String = _1
  
  def value: String
  
  def header = _1 + ": " + _2
  
  def canEqual(any: Any) = any match {
    case header: HttpHeader => true
    case _ => false
  }
  
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
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept") Some(new Accept(keyValue._2)) else None
  }

  class AcceptCharset(val value: String) extends HttpHeader {
  }
  object AcceptCharset {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-charset") Some(new AcceptCharset(keyValue._2)) else None
  }

  class AcceptEncoding(val value: String) extends HttpHeader {
  }
  object AcceptEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-encoding") Some(new AcceptEncoding(keyValue._2)) else None
  }

  class AcceptLanguage(val value: String) extends HttpHeader {
  }
  object AcceptLanguage {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-language") Some(new AcceptLanguage(keyValue._2)) else None
  }

  class AcceptRanges(val value: String) extends HttpHeader {
  }
  object AcceptRanges {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-ranges") Some(new AcceptRanges(keyValue._2)) else None
  }

  class Authorization(val value: String) extends HttpHeader {
  }
  object Authorization {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "authorization") Some(new Authorization(keyValue._2)) else None
  }

  class Connection(val value: String) extends HttpHeader {
  }
  object Connection {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "connection") Some(new Connection(keyValue._2)) else None
  }

  class Cookie(val value: String) extends HttpHeader {
  }
  object Cookie {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cookie") Some(new Cookie(keyValue._2)) else None
  }

  class ContentLength(val value: String) extends HttpHeader {
  }
  object ContentLength {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-length") Some(new ContentLength(keyValue._2)) else None
  }

  class ContentType(val value: String) extends HttpHeader {
  }
  object ContentType {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-type") Some(new ContentType(keyValue._2)) else None
  }

  class Date(val value: String) extends HttpHeader {
  }
  object Date {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "date") Some(new Date(keyValue._2)) else None
  }

  class Expect(val value: String) extends HttpHeader {
  }
  object Expect {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expect") Some(new Expect(keyValue._2)) else None
  }

  class From(val value: String) extends HttpHeader {
  }
  object From {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "from") Some(new From(keyValue._2)) else None
  }

  class Host(val value: String) extends HttpHeader {
  }
  object Host {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "host") Some(new Host(keyValue._2)) else None
  }

  class IfMatch(val value: String) extends HttpHeader {
  }
  object IfMatch {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-match") Some(new IfMatch(keyValue._2)) else None
  }

  class IfModifiedSince(val value: String) extends HttpHeader {
  }
  object IfModifiedSince {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-modified-since") Some(new IfModifiedSince(keyValue._2)) else None
  }

  class IfNoneMatch(val value: String) extends HttpHeader {
  }
  object IfNoneMatch {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-none-match") Some(new IfNoneMatch(keyValue._2)) else None
  }

  class IfRange(val value: String) extends HttpHeader {
  }
  object IfRange {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-range") Some(new IfRange(keyValue._2)) else None
  }

  class IfUnmodifiedSince(val value: String) extends HttpHeader {
  }
  object IfUnmodifiedSince {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-unmodified-since") Some(new IfUnmodifiedSince(keyValue._2)) else None
  }

  class MaxForwards(val value: String) extends HttpHeader {
  }
  object MaxForwards {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "max-forwards") Some(new MaxForwards(keyValue._2)) else None
  }

  class Pragma(val value: String) extends HttpHeader {
  }
  object Pragma {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "pragma") Some(new Pragma(keyValue._2)) else None
  }

  class ProxyAuthorization(val value: String) extends HttpHeader {
  }
  object ProxyAuthorization {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authorization") Some(new ProxyAuthorization(keyValue._2)) else None
  }

  class Range(val value: String) extends HttpHeader {
  }
  object Range {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "range") Some(new Range(keyValue._2)) else None
  }

  class Referer(val value: String) extends HttpHeader {
  }
  object Referer {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "referer") Some(new Referer(keyValue._2)) else None
  }

  class TE(val value: String) extends HttpHeader {
  }
  object TE {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "te") Some(new TE(keyValue._2)) else None
  }

  class Upgrade(val value: String) extends HttpHeader {
  }
  object Upgrade {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "upgrade") Some(new Upgrade(keyValue._2)) else None
  }

  class UserAgent(val value: String) extends HttpHeader {
  }
  object UserAgent {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "user-agent") Some(new UserAgent(keyValue._2)) else None
  }

  class Via(val value: String) extends HttpHeader {
  }
  object Via {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "via") Some(new Via(keyValue._2)) else None
  }

  class Warning(val value: String) extends HttpHeader {
  }
  object Warning {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "warning") Some(new Warning(keyValue._2)) else None
  }

  class Age(val value: String) extends HttpHeader {
  }
  object Age {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "age") Some(new Age(keyValue._2)) else None
  }

  class Allow(val value: String) extends HttpHeader {
  }
  object Allow {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "allow") Some(new Allow(keyValue._2)) else None
  }

  class CacheControl(val value: String) extends HttpHeader {
  }
  object CacheControl {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cache-control") Some(new CacheControl(keyValue._2)) else None
  }

  class ContentEncoding(val value: String) extends HttpHeader {
  }
  object ContentEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-encoding") Some(new ContentEncoding(keyValue._2)) else None
  }

  class ContentLanguage(val value: String) extends HttpHeader {
  }
  object ContentLanguage {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-language") Some(new ContentLanguage(keyValue._2)) else None
  }

  class ContentLocation(val value: String) extends HttpHeader {
  }
  object ContentLocation {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-location") Some(new ContentLocation(keyValue._2)) else None
  }

  class ContentDisposition(val value: String) extends HttpHeader {
  }
  object ContentDisposition {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-disposition") Some(new ContentDisposition(keyValue._2)) else None
  }

  class ContentMD5(val value: String) extends HttpHeader {
  }
  object ContentMD5 {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-md5") Some(new ContentMD5(keyValue._2)) else None
  }

  class ContentRange(val value: String) extends HttpHeader {
  }
  object ContentRange {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-range") Some(new ContentRange(keyValue._2)) else None
  }

  class ETag(val value: String) extends HttpHeader {
  }
  object ETag {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "etag") Some(new ETag(keyValue._2)) else None
  }

  class Expires(val value: String) extends HttpHeader {
  }
  object Expires {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expires") Some(new Expires(keyValue._2)) else None
  }

  class LastModified(val value: String) extends HttpHeader {
  }
  object LastModified {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "last-modified") Some(new LastModified(keyValue._2)) else None
  }

  class Location(val value: String) extends HttpHeader {
  }
  object Location {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "location") Some(new Location(keyValue._2)) else None
  }

  class ProxyAuthenticate(val value: String) extends HttpHeader {
  }
  object ProxyAuthenticate {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authenticate") Some(new ProxyAuthenticate(keyValue._2)) else None
  }

  class Refresh(val value: String) extends HttpHeader {
  }
  object Refresh {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "refresh") Some(new Refresh(keyValue._2)) else None
  }

  class RetryAfter(val value: String) extends HttpHeader {
  }
  object RetryAfter {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "retry-after") Some(new RetryAfter(keyValue._2)) else None
  }

  class Server(val value: String) extends HttpHeader {
  }
  object Server {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "server") Some(new Server(keyValue._2)) else None
  }

  class SetCookie(val value: String) extends HttpHeader {
  }
  object SetCookie {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "set-cookie") Some(new SetCookie(keyValue._2)) else None
  }

  class Trailer(val value: String) extends HttpHeader {
  }
  object Trailer {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "trailer") Some(new Trailer(keyValue._2)) else None
  }

  class TransferEncoding(val value: String) extends HttpHeader {
  }
  object TransferEncoding {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "transfer-encoding") Some(new TransferEncoding(keyValue._2)) else None
  }

  class Vary(val value: String) extends HttpHeader {
  }
  object Vary {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "vary") Some(new Vary(keyValue._2)) else None
  }

  class WWWAuthenticate(val value: String) extends HttpHeader {
  }
  object WWWAuthenticate {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "www-authenticate") Some(new WWWAuthenticate(keyValue._2)) else None
  }

  class XFrameOptions(val value: String) extends HttpHeader {
  }
  object XFrameOptions {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-frame-options") Some(new XFrameOptions(keyValue._2)) else None
  }

  class XXSSProtection(val value: String) extends HttpHeader {
  }
  object XXSSProtection {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-xss-protection") Some(new XXSSProtection(keyValue._2)) else None
  }

  class XContentTypeOptions(val value: String) extends HttpHeader {
  }
  object XContentTypeOptions {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-content-type-options") Some(new XContentTypeOptions(keyValue._2)) else None
  }

  class XRequestedWith(val value: String) extends HttpHeader {
  }
  object XRequestedWith {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-requested-with") Some(new XRequestedWith(keyValue._2)) else None
  }

  class XForwardedFor(val value: String) extends HttpHeader {
  }
  object XForwardedFor {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-for") Some(new XForwardedFor(keyValue._2)) else None
  }

  class XForwardedProto(val value: String) extends HttpHeader {
  }
  object XForwardedProto {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-proto") Some(new XForwardedProto(keyValue._2)) else None
  }

  class XPoweredBy(val value: String) extends HttpHeader {
  }
  object XPoweredBy {
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-powered-by") Some(new XPoweredBy(keyValue._2)) else None
  }  
}