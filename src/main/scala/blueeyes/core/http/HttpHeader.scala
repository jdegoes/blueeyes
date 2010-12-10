package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler


sealed trait HttpHeader extends Product2[String, String] with ProductPrefixUnmangler { self =>
  def _1 = unmangledName
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
    def value = mimeTypes.map(_.value).mkString(", ")
  }
  object Accept {
    def apply(mimeTypes: MimeType*): Accept = new Accept(mimeTypes: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept") 
      Some(MimeTypes.parseMimeTypes(keyValue._2)) else None
  }
  
  class `Accept-Charset`(val charSets: CharSet*) extends HttpHeader {
    def value = charSets.map(_.value).mkString(", ")
  }
  object `Accept-Charset` {
    def apply(charSets: CharSet*): `Accept-Charset` = new `Accept-Charset`(charSets: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-charset")
      Some(CharSets.parseCharSets(keyValue._2)) else None
  }

  class `Accept-Encoding`(val encodings: Encoding*) extends HttpHeader  {
    def value = encodings.map(_.value).mkString(", ")
  }
  object `Accept-Encoding` {
    def apply(encodings: Encoding*) = new `Accept-Encoding`(encodings: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-encoding") Some(Encodings.parseEncodings(keyValue._2)) else None
  }

  class `Accept-Language`(val languageRanges: LanguageRange*) extends HttpHeader {
    def value = languageRanges.map(_.value).mkString(", ");
  }
  object `Accept-Language` {
    def apply(languageRanges: LanguageRange*) = new `Accept-Language`(languageRanges: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-language") Some(LanguageRanges.parseLanguageRanges(keyValue._2)) else None
  }

  class `Accept-Ranges`(val rangeUnit: RangeUnit) extends HttpHeader {
    def value = rangeUnit.toString
  }
  object `Accept-Ranges` {
    def apply(rangeUnit: RangeUnit) = new `Accept-Ranges`(rangeUnit);
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "accept-ranges") 
      RangeUnits.parseRangeUnits(keyValue._2) else None
  }

  class Authorization(val credentials: String) extends HttpHeader {
    def value = credentials
  }
  object Authorization {
    def apply(credentials: String) = new Authorization(credentials)  // can we do better here?
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "authorization") Some(keyValue._2) else None
  }

  class Connection(val connectionToken: ConnectionToken) extends HttpHeader {
    def value = connectionToken.toString 
  }
  object Connection {
    def apply(connectionToken: ConnectionToken) = new Connection(connectionToken)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "connection")
      ConnectionTokens.parseConnectionTokens(keyValue._2) else None
  }

  class Cookie(val cookies: List[HttpCookie]) extends HttpHeader {
    val value = cookies.mkString(";")
  }
  object Cookie {
    def apply(cookies: List[HttpCookie]) = new Cookie(cookies)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cookie")
      Some(CookiesPattern(keyValue._2)) else None
  }

  class `Content-Length`(val length: HttpNumber) extends HttpHeader {
    def value = length.toString
  }
  object `Content-Length` {
    def apply(length: HttpNumber): `Content-Length` = new `Content-Length`(length)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-length")
      HttpNumbers.parseHttpNumbers(keyValue._2) else None
  }

  class `Content-Type`(val mimeTypes: MimeType*) extends HttpHeader {
    def value = mimeTypes.map(_.value).mkString(", ")
  }
  object `Content-Type` {
    def apply(mimeTypes: MimeType*) = new `Content-Type`(mimeTypes :_*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-type")
      Some(MimeTypes.parseMimeTypes(keyValue._2)) else None
  }

  class Date(val httpDate: HttpDateTime) extends HttpHeader {
    def value = httpDate.toString
  }
  object Date {
    def apply(httpDate: HttpDateTime) = new Date(httpDate)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "date")
      HttpDateTimes.parseHttpDateTimes(keyValue._2) else None
  }

  class Expect(val expectation: Expectation) extends HttpHeader {
    def value = expectation.toString
  }
  object Expect {
    def apply(expectation: Expectation) = new Expect(expectation)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expect")
      Expectations.parseExpectations(keyValue._2) else None
  }

  class From(val email: HttpUri) extends HttpHeader {
    def value = email.toString
  }
  object From {
    def apply(email: HttpUri) = new From(email)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "from")
      HttpUris.parseEmails(keyValue._2) else None
  }

  class Host(val domain: HttpUri) extends HttpHeader {
    def value = domain.host 
  }
  object Host {
    def apply(domain: HttpUri) = new Host(domain)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "host")
      HttpUris.parseHttpUris(keyValue._2) else None
  }

  /* Need to add parsing to array */
  class `If-Match`(val tags: EntityTag) extends HttpHeader {
    def value = tags.toString
  }
  object `If-Match` { // going to need a new type here
    def apply(tags: EntityTag) = new `If-Match`(tags)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-match")
      EntityTags.parseEntityTags(keyValue._2) else None
  }

  class `If-Modified-Since`(val httpDate: HttpDateTime) extends HttpHeader {
    def value = httpDate.toString
  }
  object `If-Modified-Since` {
    def apply(httpDate: HttpDateTime) = new `If-Modified-Since`(httpDate)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-modified-since")
      HttpDateTimes.parseHttpDateTimes(keyValue._2) else None
  }

  /* Need to add parsing to array */
  class `If-None-Match`(val tags: EntityTag) extends HttpHeader {
    def value = tags.toString
  }
  object `If-None-Match` {
    def apply(tags: EntityTag) = new `If-None-Match`(tags)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-none-match")
      EntityTags.parseEntityTags(keyValue._2) else None
  }

  /* If-Range needs to add a way to include the date -- probably need new class */
  class `If-Range`(val tag: IfRange) extends HttpHeader {
    def value = tag.toString 
  }
  object `If-Range` {
    def apply(tag: IfRange) = new `If-Range`(tag)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-range")
      IfRanges.parseIfRanges(keyValue._2) else None
  }


  class `If-Unmodified-Since`(val httpDate: HttpDateTime) extends HttpHeader {
    def value = httpDate.toString
  }
  object `If-Unmodified-Since` {
    def apply(httpDate: HttpDateTime) = new `If-Unmodified-Since`(httpDate)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "if-unmodified-since") 
      HttpDateTimes.parseHttpDateTimes(keyValue._2) else None
  }

  class `Max-Forwards`(val maxf: HttpNumber) extends HttpHeader {
    def value = maxf.toString 
  }
  object `Max-Forwards` {
    def apply(maxf: HttpNumber) = new `Max-Forwards`(maxf)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "max-forwards")
      HttpNumbers.parseHttpNumbers(keyValue._2) else None
  }

  class Pragma(val primeDirective: PragmaDirective) extends HttpHeader {
    def value = primeDirective.toString
  }
  object Pragma {
    def apply(primeDirective: PragmaDirective) = new Pragma(primeDirective)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "pragma")
      PragmaDirectives.parsePragmaDirectives(keyValue._2) else None
  }

  class `Proxy-Authorization`(val auth: String) extends HttpHeader {
    def value = auth
  }
  object `Proxy-Authorization` {
    def apply(auth: String) = new `Proxy-Authorization`(auth)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authorization")
      Some(keyValue._2) else None
  }

  class Range(val byteRange: ByteRange) extends HttpHeader {
    def value = byteRange.toString
  }
  object Range {
    def apply(byteRange: ByteRange) = new Range(byteRange)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "range")
      ByteRanges.parseByteRanges(keyValue._2) else None
  }

  class Referer(val domain: HttpUri) extends HttpHeader {
    def value = domain.absoluteUri
  }
  object Referer {
    def apply(domain: HttpUri) = new Referer(domain)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "referer")
      HttpUris.parseHttpUris(keyValue._2) else None
  }

  class TE(val tcodings: TCoding*) extends HttpHeader {
    def value = tcodings.map(_.toString).mkString(", ")
  }
  object TE {
    def apply(tcodings: TCoding*) = new TE(tcodings: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "te")
      Some(TCodings.parseTCodings(keyValue._2)) else None
  }

  class Upgrade(val products: ProductType*) extends HttpHeader {
    def value = products.map(_.toString).mkString(", ")
  }
  object Upgrade {
    def apply(products: ProductType*) = new Upgrade(products: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "upgrade")
      ProductTypes.parseProductTypes(keyValue._2) else None
  }

  class `User-Agent`(val product: String) extends HttpHeader {
    def value = product
  }
  object `User-Agent` {
    def apply(product: String) = new `User-Agent`(product)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "user-agent")
      Some(keyValue._2) else None
  }

  class Via(val info: ViaInfo*) extends HttpHeader {
    def value = info.map(_.toString).mkString(", ")
  }
  object Via {
    def apply(info: ViaInfo*) = new Via(info: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "via")
      Some(ViaInfos.parseViaInfos(keyValue._2)) else None
  }

  class Warning(val warnings: WarningInfo*) extends HttpHeader {
    def value = warnings.map(_.toString).mkString(", ")
  }
  object Warning {
    def apply(warnings: WarningInfo*) = new Warning(warnings: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "warning")
      Some(WarningInfos.parseWarnings(keyValue._2)) else None
  }

  /*********** Responses ************/

  class Age(val age: HttpNumber) extends HttpHeader {
    def value = age.toString
  }
  object Age {
    def apply(age: HttpNumber) = new Age(age)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "age")
      HttpNumbers.parseHttpNumbers(keyValue._2) else None
  }

  class Allow(val methods: HttpMethod*) extends HttpHeader {
    def value = methods.map(_.value).mkString(",")
  }
  object Allow {
    def apply(methods: HttpMethod*) = new Allow(methods: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "allow")
      Some(HttpMethods.parseHttpMethods(keyValue._2)) else None
  }

  class `Cache-Control`(val directives: CacheDirective*) extends HttpHeader {
    def value = directives.map(_.toString).mkString(", ") 
  }
  object `Cache-Control` {
    def apply(directives: CacheDirective*) = new `Cache-Control`(directives: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "cache-control")
      Some(CacheDirectives.parseCacheDirectives(keyValue._2)) else None
  }

  class `Content-Encoding`(val encodings: Encoding*) extends HttpHeader {
    def value = encodings.map(_.toString).mkString(", ")
  }
  object `Content-Encoding` {
    def apply(encodings: Encoding*) = new `Content-Encoding`(encodings: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-encoding")
      Some(Encodings.parseEncodings(keyValue._2)) else None
  }

  class `Content-Language`(languageRanges: LanguageRange*) extends HttpHeader {
    def value = languageRanges.map(_.toString).mkString(", ")
  }
  object `Content-Language` {
    def apply(languageRanges: LanguageRange*) = new `Content-Language`(languageRanges: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-language")
      Some(LanguageRanges.parseLanguageRanges(keyValue._2)) else None
  }

  /* Content-Location: An alternate location for the returned data -- maybe use a URI/URL parser?
   * .. I Think this is referring to the path of the URL
   */
  class `Content-Location`(val value: String) extends HttpHeader {
  }
  object `Content-Location` {
    def apply(location: String) = new `Content-Location`(location)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-location") Some(keyValue._2) else None
  }

  class `Content-Disposition`(val disposition: DispositionType) extends HttpHeader {
    def value = disposition.toString
  }
  object `Content-Disposition` {
    def apply(disposition: DispositionType) = new `Content-Disposition`(disposition)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-disposition")
      Some(DispositionTypes.parseDispositionTypes(keyValue._2)) else None
  }

  class `Content-MD5`(val value: String) extends HttpHeader {
  }
  object `Content-MD5` {
    def apply(hash: String) = new `Content-MD5`(hash)  
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-md5") Some(keyValue._2) else None
  }

  class `Content-Range`(val byteRange: ContentByteRange) extends HttpHeader {
    def value = byteRange.toString  
  }
  object `Content-Range` {
    def apply(byteRange: ContentByteRange) = new `Content-Range`(byteRange)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "content-range") 
      ContentByteRanges.parseContentByteRanges(keyValue._2) else None
  }

  class ETag(val tag: EntityTag) extends HttpHeader {
    def value = tag.toString
  }
  object ETag {
    def apply(tag: EntityTag) = new ETag(tag)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "etag")
      EntityTags.parseEntityTags(keyValue._2) else None
  }

  class Expires(val date: HttpDateTime) extends HttpHeader {
    def value = date.toString
  }
  object Expires {
    def apply(date: HttpDateTime) = new Expires(date)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "expires")
      HttpDateTimes.parseHttpDateTimes(keyValue._2) else None
  }

  class `Last-Modified`(val date: HttpDateTime) extends HttpHeader {
    def value = date.toString
  }
  object `Last-Modified` {
    def apply(date: HttpDateTime) = new `Last-Modified`(date)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "last-modified")
      HttpDateTimes.parseHttpDateTimes(keyValue._2) else None
  }

  class Location(val domain: HttpUri) extends HttpHeader {
    def value = domain.absoluteUri
  }
  object Location {
    def apply(domain: HttpUri) = new Location(domain)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "location")
      HttpUris.parseHttpUris(keyValue._2) else None
  }

  class `Proxy-Authenticate`(val challenge: String) extends HttpHeader {
    def value = challenge
  }
  object `Proxy-Authenticate` {
    def apply(challenge: String) = new `Proxy-Authenticate`(challenge)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "proxy-authenticate") 
      Some(keyValue._2) else None
  }

  /* Sometimes contains a Url --  Will need to change this */
  class Refresh(val time: HttpNumber) extends HttpHeader {
    def value = time.toString
  }
  object Refresh {
    def apply(time: HttpNumber) = new Refresh(time)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "refresh")
      HttpNumbers.parseHttpNumbers(keyValue._2) else None
  }

  /* Could also be a date -- will need to change this */
  class `Retry-After`(val num: HttpNumber) extends HttpHeader {
    def value = num.toString
  }
  object `Retry-After` {
    def apply(num: HttpNumber) = new `Retry-After`(num)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "retry-after")
      HttpNumbers.parseHttpNumbers(keyValue._2) else None
  }

  /* Server comments can be almost anything */
  class Server(val comment: String) extends HttpHeader {
    def value = comment
  }
  object Server {
    def apply(comment: String) = new Server(comment)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "server") Some(keyValue._2) else None
  }

  class `Set-Cookie`(val cookies: List[HttpCookie]) extends HttpHeader {
    val value = cookies.mkString(";")
  }
  object `Set-Cookie` {
    def apply(cookies: List[HttpCookie]) = new `Set-Cookie`(cookies)
    def unapply(keyValue: (String, String)): Option[List[HttpCookie]] = if (keyValue._1.toLowerCase == "set-cookie")
      Some(CookiesPattern(keyValue._2)) else None
  }

  /* Will take a while to implement */
  class Trailer(val fields: HttpHeaderField*) extends HttpHeader {
    def value = fields.map(_.toString).mkString(", ")
  }
  object Trailer {
    def apply(fields: HttpHeaderField*) = new Trailer(fields: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "trailer")
      HttpHeaderFields.parseHttpHeaderFields(keyValue._2, "trailer") else None
  }

  class `Transfer-Encoding`(val encodings: Encoding*) extends HttpHeader {
    def value = encodings.map(_.toString).mkString(", ")
  }
  object `Transfer-Encoding` {
    def apply(encodings: Encoding*) = new `Transfer-Encoding`(encodings: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "transfer-encoding")
      Some(Encodings.parseEncodings(keyValue._2)) else None
  }

  /* There are problems with using Vary in IE.  */
  class Vary(val value: String) extends HttpHeader {
  }
  object Vary {
    def apply(value: String) = new Vary(value)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "vary") Some(keyValue._2) else None
  }

  class `WWW-Authenticate`(val challenge: String) extends HttpHeader {
    def value = challenge
  }
  object `WWW-Authenticate` {
    def apply(challenge: String) = new `WWW-Authenticate`(challenge)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "www-authenticate") Some(keyValue._2) else None
  }

  class `X-Frame-Options`(val option: FrameOption) extends HttpHeader {
    def value = option.toString
  }
  object `X-Frame-Options` {
    def apply(option: FrameOption) = new `X-Frame-Options`(option)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-frame-options")
      FrameOptions.parseFrameOptions(keyValue._2) else None
  }

  /* X-XSS-Protection Seems to be primarily used by IE8 */
  class `X-XSS-Protection`(val xss: String) extends HttpHeader {
    def value = xss;
  }
  object `X-XSS-Protection` {
    def apply(xss: String) = new `X-XSS-Protection`(xss)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-xss-protection")
        Some(keyValue._2) else None
  }

  class `X-Content-Type-Options`(val option: ContentTypeOption) extends HttpHeader {
    def value = option.toString
  }
  object `X-Content-Type-Options` {
    def apply(option: ContentTypeOption) = new `X-Content-Type-Options`(option)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-content-type-options")
      ContentTypeOptions.parseContentTypeOptions(keyValue._2) else None
  }

  class `X-Requested-With`(val requested: RequestedWith) extends HttpHeader {
    def value = requested.toString
  }
  object `X-Requested-With` {
    def apply(requested: RequestedWith) = new `X-Requested-With`(requested)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-requested-with")
      RequestedWiths.parseRequestedWiths(keyValue._2) else None
  }

  class `X-Forwarded-For`(val ips: HttpIp*) extends HttpHeader {
    def value = ips.map(_.toString).mkString(", ")
  }
  object `X-Forwarded-For` {
    def apply (ips: HttpIp*) = new `X-Forwarded-For`(ips: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-for") 
      Some(HttpIps.parseHttpIps(keyValue._2)) else None
  }

  class `X-Forwarded-Proto`(val proto: String) extends HttpHeader {
    def value = proto
  }
  object `X-Forwarded-Proto` {
    def apply(proto: String) = new `X-Forwarded-Proto`(proto)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-forwarded-proto")
      Some(keyValue._2) else None
  }

  class `X-Powered-By`(val products: ProductType*) extends HttpHeader {
    def value = products.map(_.toString).mkString(",")
  }
  object `X-Powered-By` {
    def apply(products: ProductType*) = new `X-Powered-By`(products: _*)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "x-powered-by") 
      ProductTypes.parseProductTypes(keyValue._2) else None
  }  

  /* Very new headers */
  class `Access-Control-Allow-Origin`(val origin: String) extends HttpHeader {
    def value = origin
  }
  object `Access-Control-Allow-Origin` {
    def apply(origin: String) = new `Access-Control-Allow-Origin`(origin)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "access-control-allow-origin") 
      Some(keyValue._2) else None
  }

  class `Access-Control-Request-Method`(val methods: HttpMethod*) extends HttpHeader {
    def value = methods.map(_.toString).mkString(",") 
  }
  object `Access-Control-Request-Method` {
    def apply(methods: HttpMethod*) = new `Access-Control-Request-Method`(methods:_ *)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "access-control-request-method") 
      Some(HttpMethods.parseHttpMethods(keyValue._2)) else None
  }

  class `Access-Control-Request-Headers`(val fields: HttpHeaderField*) extends HttpHeader {
    def value = fields.map(_.toString).mkString(",")
  }
  object `Access-Control-Request-Headers` {
    def apply(fields: HttpHeaderField*) = new `Access-Control-Request-Headers`(fields:_ *)
    def unapply(keyValue: (String, String)) = if (keyValue._1.toLowerCase == "access-control-request-headers") 
      HttpHeaderFields.parseHttpHeaderFields(keyValue._2, "accessControl") else None
  }
  
  class CustomHeader(override val name: String, val value: String) extends HttpHeader {
  }
}

trait HttpHeaderImplicits {
  import HttpHeaders._
  
  implicit def httpHeader2Tuple(httpHeader: HttpHeader) = (httpHeader._1, httpHeader._2)
  
  implicit def tuple2HttpHeader(keyValue: (String, String)): HttpHeader = keyValue match {
    case Accept(value) => new Accept(value: _*)
    case `Accept-Charset`(value) => new `Accept-Charset`(value: _*)
    case `Accept-Encoding`(value) => new `Accept-Encoding`(value: _*)
    case `Accept-Language`(value) => new `Accept-Language`(value: _*)
    case `Accept-Ranges`(value) => new `Accept-Ranges`(value)
    case Authorization(value) => new Authorization(value)
    case Connection(value) => new Connection(value)
    case Cookie(value) => new Cookie(value)
    case `Content-Length`(value) => new `Content-Length`(value)
    case `Content-Type`(value) => new `Content-Type`(value: _*)
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
    case TE(value) => new TE(value: _*)
    case Upgrade(value) => new Upgrade(value: _*)
    case `User-Agent`(value) => new `User-Agent`(value)
    case Via(value) => new Via(value: _*)
    case Warning(value) => new Warning(value: _*)

    /** Responses **/
    case Age(value) => new Age(value)
    case Allow(value) => new Allow(value: _*)
    case `Cache-Control`(value) => new `Cache-Control`(value: _*)
    case `Content-Encoding`(value) => new `Content-Encoding`(value: _*)
    case `Content-Language`(value) => new `Content-Language`(value: _*)
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
    case Trailer(value) => new Trailer(value: _*)
    case `Transfer-Encoding`(value) => new `Transfer-Encoding`(value: _*)
    case Vary(value) => new Vary(value)
    case `WWW-Authenticate`(value) => new `WWW-Authenticate`(value)
    case `X-Frame-Options`(value) => new `X-Frame-Options`(value)
    case `X-XSS-Protection`(value) => new `X-XSS-Protection`(value)
    case `X-Content-Type-Options`(value) => new `X-Content-Type-Options`(value)
    case `X-Requested-With`(value) => new `X-Requested-With`(value)
    case `X-Forwarded-For`(value) => new `X-Forwarded-For`(value: _*)
    case `X-Forwarded-Proto`(value) => new `X-Forwarded-Proto`(value)
    case `X-Powered-By`(value) => new `X-Powered-By`(value: _*)
    case `Access-Control-Allow-Origin`(value) => new `Access-Control-Allow-Origin`(value)
    case `Access-Control-Request-Method`(value) => new `Access-Control-Request-Method`(value: _*)
    case `Access-Control-Request-Headers`(value) => new `Access-Control-Request-Headers`(value: _*)
    case (name, value) => new CustomHeader(name, value)
  }
  
  implicit def headersList2HeadersMap(headers: Seq[HttpHeader]): Map[String, String] = Map(headers.map(header => {header: (String, String)}): _*)
}
object HttpHeaderImplicits extends HttpHeaderImplicits
