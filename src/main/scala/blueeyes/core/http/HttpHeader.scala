package blueeyes.core.http

import blueeyes.util.ProductPrefixUnmangler
import scalaz.Scalaz._
import scala.util.parsing.combinator._
import scala.util.parsing.input._

sealed trait HttpHeaderField[T <: HttpHeader] extends ProductPrefixUnmangler {
  lazy val name: String = unmangledName

  def parse(s: String): Option[T]
  def parse(t: (String, String)): Option[T] = 
    if (t._1.equalsIgnoreCase(name)) parse(t._2) else None

  override def toString = name
}

object HttpHeaderField {
  import HttpHeaders._

  val All: List[HttpHeaderField[_ <: HttpHeader]] = List(
    Accept,
    `Accept-Charset`,
    `Accept-Encoding`,
    `Accept-Language`,
    `Accept-Ranges`,
    Authorization,
    Connection,
    Cookie,
    `Content-Length`,
    `Content-Type`,
    Date,
    Expect,
    From,
    Host,
    `If-Match`,
    `If-Modified-Since`,
    `If-None-Match`,
    `If-Range`,
    `If-Unmodified-Since`,
    `Max-Forwards`,
    Pragma,
    `Proxy-Authorization`,
    Range,
    Referer,
    TE,
    Upgrade,
    `User-Agent`,
    Via,
    Warning,

    /** Responses **/
    Age,
    Allow,
    `Cache-Control`,
    `Content-Encoding`,
    `Content-Language`,
    `Content-Location`,
    `Content-Disposition`,
    `Content-MD5`,
    `Content-Range`,
    ETag,
    Expires,
    `Last-Modified`,
    Location,
    `Proxy-Authenticate`,
    Refresh,
    `Retry-After`,
    Server,
    `Set-Cookie`,
    Trailer,
    `Transfer-Encoding`,
    Vary,
    `WWW-Authenticate`,
    `X-Frame-Options`,
    `X-XSS-Protection`,
    `X-Content-Type-Options`,
    `X-Requested-With`,
    `X-Forwarded-For`,
    `X-Cluster-Client-Ip`,
    `X-Forwarded-Proto`,
    `X-Powered-By`,
    `Access-Control-Allow-Origin`,
    `Access-Control-Request-Method`,
    `Access-Control-Request-Headers`
  )

  val ByName: Map[String, HttpHeaderField[_]] = All.map(v => (v.name.toLowerCase -> v)).toMap

  def parseAll(inString: String, parseType: String): List[HttpHeaderField[_]] = {
    val fields = inString.toLowerCase.split("""\s*,\s*""").toList.flatMap(HttpHeaderField.ByName.get)

    if (parseType == "trailer") fields.filterNot {
      case Trailer => true
      case `Transfer-Encoding` => true
      case `Content-Length` => true
      case NullHeader => true
      case _ => false
    } else fields.filterNot {
      case NullHeader => true
      case _ => false 
    }
  }
}

sealed trait HttpHeader extends ProductPrefixUnmangler {
  def name: String = unmangledName
  def value: String

  def header = name + ": " + value

  def canEqual(any: Any) = any match {
    case header: HttpHeader => true
    case _ => false
  }

  override def productPrefix = this.getClass.getSimpleName

  override def toString = header

  def tuple = (name, value)

  override def equals(any: Any) = any match {
    case that: HttpHeader => name.equalsIgnoreCase(that.name) && value.equalsIgnoreCase(that.value)
    case _ => false
  }
}

trait HttpHeaderImplicits {
  import HttpHeaders._

  implicit def tuple2HttpHeader(t: (String, String)): HttpHeader = HttpHeader(t)
}

object HttpHeader extends HttpHeaderImplicits {
  def apply(t: (String, String)): HttpHeader = {
    def extract(l: List[HttpHeaderField[_ <: HttpHeader]]): HttpHeader = l match {
      case x :: xs => x.parse(t).getOrElse(extract(xs))
      case Nil => HttpHeaders.CustomHeader(t._1, t._2)
    }

    extract(HttpHeaderField.All)
  }
}

sealed trait HttpHeaderRequest extends HttpHeader
sealed trait HttpHeaderResponse extends HttpHeader

case class HttpHeaders private (val raw: Map[String, String]) {
  def + (kv: (String, String)): HttpHeaders = new HttpHeaders(raw + HttpHeader(kv).tuple)
  def + (header: HttpHeader): HttpHeaders = new HttpHeaders(raw + header.tuple)
  def ++ (that: HttpHeaders) = new HttpHeaders(raw ++ that.raw)

  def - (key: String): HttpHeaders = new HttpHeaders(raw - key)

  def get(key: String): Option[String] = raw.get(key)

  def header[T <: HttpHeader](implicit field: HttpHeaderField[T]): Option[T] = {
    raw.get(field.name).flatMap(field.parse)
  }
}

trait HttpHeadersImplicits extends HttpHeaderImplicits {
  implicit def iterableOfTuple2ToHttpHeaders(i: Iterable[(String, String)]): HttpHeaders = HttpHeaders(i)
  implicit def iterableToHttpHeaders[A <: HttpHeader](i: Iterable[A]): HttpHeaders = HttpHeaders(i)
}

object HttpHeaders {
  def apply(i: Iterable[(String, String)]): HttpHeaders = {
    new HttpHeaders(i.map(HttpHeader(_).tuple)(collection.breakOut))
  }

  def apply[A](i: Iterable[A])(implicit ev: A <:< HttpHeader): HttpHeaders = {
    new HttpHeaders(i.map(ev(_).tuple)(collection.breakOut))
  }

  val Empty: HttpHeaders = new HttpHeaders(Map.empty[String, String])

  /************ Requests ************/

  case class Accept(mimeTypes: MimeType*) extends HttpHeaderRequest {
    def value = mimeTypes.map(_.value).mkString(", ")
  }
  implicit case object Accept extends HttpHeaderField[Accept] {
    override def parse(s: String) = Some(apply(MimeTypes.parseMimeTypes(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.mimeTypes)
  }

  case class `Accept-Charset`(charSets: CharSet*) extends HttpHeaderRequest {
    def value = charSets.map(_.value).mkString(", ")
  }
  implicit case object `Accept-Charset` extends HttpHeaderField[`Accept-Charset`] {
    override def parse(s: String) = Some(apply(CharSets.parseCharSets(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.charSets)
  }

  case class `Accept-Encoding`(encodings: Encoding*) extends HttpHeaderRequest  {
    def value = encodings.map(_.value).mkString(", ")
  }
  implicit case object `Accept-Encoding` extends HttpHeaderField[`Accept-Encoding`] {
    override def parse(s: String) = Some(apply(Encodings.parseEncodings(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.encodings)
  }

  case class `Accept-Language`(languageRanges: LanguageRange*) extends HttpHeaderRequest {
    def value = languageRanges.map(_.value).mkString(", ");
  }
  implicit case object `Accept-Language` extends HttpHeaderField[`Accept-Language`] {
    override def parse(s: String) = Some(apply(LanguageRanges.parseLanguageRanges(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.languageRanges)
  }

  case class `Accept-Ranges`(rangeUnit: RangeUnit) extends HttpHeaderResponse {
    def value = rangeUnit.toString
  }
  implicit case object `Accept-Ranges` extends HttpHeaderField[`Accept-Ranges`] {
    override def parse(s: String) = RangeUnits.parseRangeUnits(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.rangeUnit)
  }

  case class Authorization(credentials: String) extends HttpHeaderRequest {
    def value = credentials
  }
  implicit case object Authorization extends HttpHeaderField[Authorization] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.credentials)
  }

  case class Connection(connectionToken: ConnectionToken) extends HttpHeaderRequest {
    def value = connectionToken.toString
  }
  implicit case object Connection extends HttpHeaderField[Connection] {
    override def parse(s: String) = ConnectionTokens.parseConnectionTokens(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.connectionToken)
  }

  case class Cookie(cookies: List[HttpCookie]) extends HttpHeaderRequest {
    def value = cookies.mkString(";")
  }
  implicit case object Cookie extends HttpHeaderField[Cookie] {
    override def parse(s: String) = Some(apply(CookiesPattern(s)))
    def unapply(t: (String, String)) = parse(t).map(_.cookies)
  }

  object Cookies {
    def unapply(t: (String, String)) = Cookie.parse(t)
  }

  case class `Content-Length`(length: Long) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = length.toString
  }
  implicit case object `Content-Length` extends HttpHeaderField[`Content-Length`] {
    override def parse(s: String) = s.parseLong.toOption.map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.length)
  }

  case class `Content-Type`(mimeTypes: MimeType*) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = mimeTypes.map(_.value).mkString(", ")
  }
  implicit case object `Content-Type` extends HttpHeaderField[`Content-Type`] {
    override def parse(s: String) = Some(apply(MimeTypes.parseMimeTypes(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.mimeTypes)
  }

  case class Date(httpDate: HttpDateTime) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = httpDate.toString
  }
  implicit case object Date extends HttpHeaderField[Date] {
    override def parse(s: String) = HttpDateTimes.parseHttpDateTimes(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.httpDate)
  }

  case class Expect(expectation: Expectation) extends HttpHeaderRequest {
    def value = expectation.toString
  }
  implicit case object Expect extends HttpHeaderField[Expect] {
    override def parse(s: String) = Expectations.parseExpectations(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.expectation)
  }

  case class From(email: Email) extends HttpHeaderRequest {
    def value = email.toString
  }
  implicit case object From extends HttpHeaderField[From] {
    override def parse(s: String) = Emails(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.email)
  }

  case class Host(domain: URI) extends HttpHeaderRequest {
    def value = List(domain.host, domain.port.map(":" + _)).map(_.getOrElse("")).mkString("")
  }
  implicit case object Host extends HttpHeaderField[Host] {
    override def parse(s: String) = URI.opt("http://" + s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.domain)
  }

  /* Need to add parsing to array */
  case class `If-Match`(tags: EntityTag) extends HttpHeaderRequest {
    def value = tags.toString
  }
  implicit case object `If-Match` extends HttpHeaderField[`If-Match`] { // going to need a new type here
    override def parse(s: String) = EntityTags.parseEntityTags(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.tags)
  }

  case class `If-Modified-Since`(httpDate: HttpDateTime) extends HttpHeaderRequest {
    def value = httpDate.toString
  }
  implicit case object `If-Modified-Since` extends HttpHeaderField[`If-Modified-Since`] {
    override def parse(s: String) = HttpDateTimes.parseHttpDateTimes(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.httpDate)
  }

  /* Need to add parsing to array */
  case class `If-None-Match`(tags: EntityTag) extends HttpHeaderRequest {
    def value = tags.toString
  }
  implicit case object `If-None-Match` extends HttpHeaderField[`If-None-Match`] {
    override def parse(s: String) = EntityTags.parseEntityTags(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.tags)
  }

  /* If-Range needs to add a way to include the date -- probably need new class */
  case class `If-Range`(tag: IfRange) extends HttpHeaderRequest {
    def value = tag.toString
  }
  implicit case object `If-Range` extends HttpHeaderField[`If-Range`] {
    override def parse(s: String) = IfRanges.parseIfRanges(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.tag)
  }

  case class `If-Unmodified-Since`(httpDate: HttpDateTime) extends HttpHeaderRequest {
    def value = httpDate.toString
  }
  implicit case object `If-Unmodified-Since` extends HttpHeaderField[`If-Unmodified-Since`] {
    override def parse(s: String) = HttpDateTimes.parseHttpDateTimes(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.httpDate)
  }

  case class `Max-Forwards`(maxf: HttpNumber) extends HttpHeaderRequest {
    def value = maxf.toString
  }
  implicit case object `Max-Forwards` extends HttpHeaderField[`Max-Forwards`] {
    override def parse(s: String) = HttpNumbers.parseHttpNumbers(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.maxf)
  }

  case class Pragma(primeDirective: PragmaDirective) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = primeDirective.toString
  }
  implicit case object Pragma extends HttpHeaderField[Pragma] {
    override def parse(s: String) = PragmaDirectives.parsePragmaDirectives(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.primeDirective)
  }

  case class `Proxy-Authorization`(auth: String) extends HttpHeaderRequest {
    def value = auth
  }
  implicit case object `Proxy-Authorization` extends HttpHeaderField[`Proxy-Authorization`] {
    override def parse(s: String) = Some(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.auth)
  }

  case class Range(byteRange: ByteRange) extends HttpHeaderRequest {
    def value = byteRange.toString
  }
  implicit case object Range extends HttpHeaderField[Range] {
    override def parse(s: String) = ByteRanges.parseByteRanges(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.byteRange)
  }

  case class Referer(domain: URI) extends HttpHeaderRequest {
    def value = domain.toString
  }
  implicit case object Referer extends HttpHeaderField[Referer] {
    override def parse(s: String) = URI.opt(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.domain)
  }

  case class TE(tcodings: TCoding*) extends HttpHeaderRequest {
    def value = tcodings.map(_.toString).mkString(", ")
  }
  implicit case object TE extends HttpHeaderField[TE] {
    override def parse(s: String) = Some(apply(TCodings.parseTCodings(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.tcodings)
  }

  case class Upgrade(products: ProductType*) extends HttpHeaderRequest {
    def value = products.map(_.toString).mkString(", ")
  }
  implicit case object Upgrade extends HttpHeaderField[Upgrade] {
    override def parse(s: String) = ProductTypes.parseProductTypes(s).map(apply(_: _*))
    def unapply(t: (String, String)) = parse(t).map(_.products)
  }

  case class `User-Agent`(product: String) extends HttpHeaderRequest {
    def value = product
  }
  implicit case object `User-Agent` extends HttpHeaderField[`User-Agent`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.product)
  }

  case class Via(info: ViaInfo*) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = info.map(_.toString).mkString(", ")
  }
  implicit case object Via extends HttpHeaderField[Via] {
    override def parse(s: String) = Some(apply(ViaInfos.parseViaInfos(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.info)
  }

  case class Warning(warnings: WarningInfo*) extends HttpHeaderRequest with HttpHeaderResponse {
    def value = warnings.map(_.toString).mkString(", ")
  }
  implicit case object Warning extends HttpHeaderField[Warning] {
    override def parse(s: String) = Some(apply(WarningInfos.parseWarnings(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.warnings)
  }

  /*********** Responses ************/

  case class Age(age: Double) extends HttpHeaderResponse {
    def value = age.toString
  }
  implicit case object Age extends HttpHeaderField[Age] {
    override def parse(s: String) = s.parseDouble.toOption.map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.age)
  }

  case class Allow(methods: HttpMethod*) extends HttpHeaderResponse {
    def value = methods.map(_.value).mkString(",")
  }
  implicit case object Allow extends HttpHeaderField[Allow] {
    override def parse(s: String) = Some(apply(HttpMethods.parseHttpMethods(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.methods)
  }

  case class `Cache-Control`(directives: CacheDirective*) extends HttpHeaderResponse {
    def value = directives.map(_.toString).mkString(", ")
  }
  implicit case object `Cache-Control` extends HttpHeaderField[`Cache-Control`] {
    override def parse(s: String) = Some(apply(CacheDirectives.parseCacheDirectives(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.directives)
  }

  case class `Content-Encoding`(encodings: Encoding*) extends HttpHeaderResponse {
    def value = encodings.map(_.toString).mkString(", ")
  }
  implicit case object `Content-Encoding` extends HttpHeaderField[`Content-Encoding`] {
    override def parse(s: String) = Some(apply(Encodings.parseEncodings(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.encodings)
  }

  case class `Content-Language`(languageRanges: LanguageRange*) extends HttpHeaderResponse {
    def value = languageRanges.map(_.toString).mkString(", ")
  }
  implicit case object `Content-Language` extends HttpHeaderField[`Content-Language`] {
    override def parse(s: String) = Some(apply(LanguageRanges.parseLanguageRanges(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.languageRanges)
  }

  /* Content-Location: An alternate location for the returned data -- maybe use a URI/URL parser?
   * .. I Think this is referring to the path of the URL
   */
  case class `Content-Location`(value: String) extends HttpHeaderResponse {
  }
  implicit case object `Content-Location` extends HttpHeaderField[`Content-Location`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.value)
  }

  case class `Content-Disposition`(disposition: DispositionType) extends HttpHeaderResponse {
    def value = disposition.toString
  }
  implicit case object `Content-Disposition` extends HttpHeaderField[`Content-Disposition`] {
    override def parse(s: String) = Some(apply(DispositionTypes.parseDispositionTypes(s)))
    def unapply(t: (String, String)) = parse(t).map(_.disposition)
  }

  case class `Content-MD5`(value: String) extends HttpHeaderRequest with HttpHeaderResponse 
  implicit case object `Content-MD5` extends HttpHeaderField[`Content-MD5`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.value)
  }

  case class `Content-Range`(byteRange: ContentByteRange) extends HttpHeaderResponse {
    def value = byteRange.toString
  }
  implicit case object `Content-Range` extends HttpHeaderField[`Content-Range`] {
    override def parse(s: String) = ContentByteRanges.parseContentByteRanges(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.byteRange)
  }

  case class ETag(tag: EntityTag) extends HttpHeaderResponse {
    def value = tag.toString
  }
  implicit case object ETag extends HttpHeaderField[ETag] {
    override def parse(s: String) = EntityTags.parseEntityTags(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.tag)
  }

  case class Expires(date: HttpDateTime) extends HttpHeaderResponse {
    def value = date.toString
  }
  implicit case object Expires extends HttpHeaderField[Expires] {
    override def parse(s: String) = HttpDateTimes.parseHttpDateTimes(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.date)
  }

  case class `Last-Modified`(date: HttpDateTime) extends HttpHeaderResponse {
    def value = date.toString
  }
  implicit case object `Last-Modified` extends HttpHeaderField[`Last-Modified`] {
    override def parse(s: String) = HttpDateTimes.parseHttpDateTimes(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.date)
  }

  case class Location(domain: URI) extends HttpHeaderResponse {
    def value = domain.toString
  }
  implicit case object Location extends HttpHeaderField[Location] {
    override def parse(s: String) = URI.opt(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.domain)
  }

  case class `Proxy-Authenticate`(challenge: String) extends HttpHeaderResponse {
    def value = challenge
  }
  implicit case object `Proxy-Authenticate` extends HttpHeaderField[`Proxy-Authenticate`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.challenge)
  }

  /* Sometimes contains a Url --  Will need to change this */
  case class Refresh(time: HttpNumber) extends HttpHeaderResponse {
    def value = time.toString
  }
  implicit case object Refresh extends HttpHeaderField[Refresh] {
    override def parse(s: String) = HttpNumbers.parseHttpNumbers(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.time)
  }

  /* Could also be a date -- will need to change this */
  case class `Retry-After`(num: HttpNumber) extends HttpHeaderResponse {
    def value = num.toString
  }
  implicit case object `Retry-After` extends HttpHeaderField[`Retry-After`] {
    override def parse(s: String) = HttpNumbers.parseHttpNumbers(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.num)
  }

  /* Server comments can be almost anything */
  case class Server(comment: String) extends HttpHeaderResponse {
    def value = comment
  }
  implicit case object Server extends HttpHeaderField[Server] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.comment)
  }

  case class `Set-Cookie`(cookies: List[HttpCookie]) extends HttpHeaderResponse {
    def value = cookies.mkString(";")
  }
  implicit case object `Set-Cookie` extends HttpHeaderField[`Set-Cookie`] {
    override def parse(s: String) = Some(apply(CookiesPattern(s)))
    def unapply(t: (String, String)) = parse(t).map(_.cookies)
  }

  /* Will take a while to implement */
  case class Trailer(fields: HttpHeaderField[_]*) extends HttpHeaderResponse {
    def value = fields.map(_.toString).mkString(", ")
  }
  implicit case object Trailer extends HttpHeaderField[Trailer] {
    override def parse(s: String) = Some(apply(HttpHeaderField.parseAll(s, "trailer"): _*))
    def unapply(t: (String, String)) = parse(t).map(_.fields)
  }

  case class `Transfer-Encoding`(encodings: Encoding*) extends HttpHeaderResponse {
    def value = encodings.map(_.toString).mkString(", ")
  }
  implicit case object `Transfer-Encoding` extends HttpHeaderField[`Transfer-Encoding`] {
    override def parse(s: String) = Some(apply(Encodings.parseEncodings(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.encodings)
  }

  /* There are problems with using Vary in IE.  */
  case class Vary(value: String) extends HttpHeaderResponse 
  implicit case object Vary extends HttpHeaderField[Vary] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.value)
  }

  case class `WWW-Authenticate`(challenge: String) extends HttpHeaderResponse {
    def value = challenge
  }
  implicit case object `WWW-Authenticate` extends HttpHeaderField[`WWW-Authenticate`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.challenge)
  }

  case class `X-Frame-Options`(option: FrameOption) extends HttpHeaderResponse {
    def value = option.toString
  }
  implicit case object `X-Frame-Options` extends HttpHeaderField[`X-Frame-Options`] {
    override def parse(s: String) = FrameOptions.parseFrameOptions(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.option)
  }

  /* X-XSS-Protection Seems to be primarily used by IE8 */
  case class `X-XSS-Protection`(xss: String) extends HttpHeaderResponse {
    def value = xss;
  }
  implicit case object `X-XSS-Protection` extends HttpHeaderField[`X-XSS-Protection`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.xss)
  }

  case class `X-Content-Type-Options`(option: ContentTypeOption) extends HttpHeaderResponse {
    def value = option.toString
  }
  implicit case object `X-Content-Type-Options` extends HttpHeaderField[`X-Content-Type-Options`] {
    override def parse(s: String) = ContentTypeOptions.parseContentTypeOptions(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.option)
  }

  case class `X-Requested-With`(requested: RequestedWith) extends HttpHeaderRequest {
    def value = requested.toString
  }
  implicit case object `X-Requested-With` extends HttpHeaderField[`X-Requested-With`] {
    override def parse(s: String) = RequestedWiths.parseRequestedWiths(s).map(apply)
    def unapply(t: (String, String)) = parse(t).map(_.requested)
  }

  case class `X-Forwarded-For`(ips: HttpIp*) extends HttpHeaderRequest {
    def value = ips.map(_.toString).mkString(", ")
  }
  implicit case object `X-Forwarded-For` extends HttpHeaderField[`X-Forwarded-For`] {
    override def parse(s: String) = Some(apply(HttpIps.parseHttpIps(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.ips)
  }

  case class `X-Cluster-Client-Ip`(ips: HttpIp*) extends HttpHeaderRequest {
    def value = ips.map(_.toString).mkString(", ")
  }
  implicit case object `X-Cluster-Client-Ip` extends HttpHeaderField[`X-Cluster-Client-Ip`] {
    override def parse(s: String) = Some(apply(HttpIps.parseHttpIps(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.ips)
  }

  case class `X-Forwarded-Proto`(proto: String) extends HttpHeaderRequest {
    def value = proto
  }
  implicit case object `X-Forwarded-Proto` extends HttpHeaderField[`X-Forwarded-Proto`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.proto)
  }

  case class `X-Powered-By`(products: ProductType*) extends HttpHeaderRequest {
    def value = products.map(_.toString).mkString(",")
  }
  implicit case object `X-Powered-By` extends HttpHeaderField[`X-Powered-By`] {
    override def parse(s: String) = ProductTypes.parseProductTypes(s).map(apply(_: _*))
    def unapply(t: (String, String)) = parse(t).map(_.products)
  }

  /* Very new headers */
  case class `Access-Control-Allow-Origin`(origin: String) extends HttpHeaderResponse {
    def value = origin
  }
  implicit case object `Access-Control-Allow-Origin` extends HttpHeaderField[`Access-Control-Allow-Origin`] {
    override def parse(s: String) = Some(apply(s))
    def unapply(t: (String, String)) = parse(t).map(_.origin)
  }

  case class `Access-Control-Request-Method`(methods: HttpMethod*) extends HttpHeaderRequest {
    def value = methods.map(_.toString).mkString(",")
  }
  implicit case object `Access-Control-Request-Method` extends HttpHeaderField[`Access-Control-Request-Method`] {
    override def parse(s: String) = Some(apply(HttpMethods.parseHttpMethods(s): _*))
    def unapply(t: (String, String)) = parse(t).map(_.methods)
  }

  case class `Access-Control-Request-Headers`(fields: HttpHeaderField[_]*) extends HttpHeaderRequest {
    def value = fields.map(_.toString).mkString(",")
  }
  implicit case object `Access-Control-Request-Headers` extends HttpHeaderField[`Access-Control-Request-Headers`] {
    override def parse(s: String) = Some(apply(HttpHeaderField.parseAll(s, "accessControl"): _*))
    def unapply(t: (String, String)) = parse(t).map(_.fields)
  }

  case class CustomHeader(override val name: String, val value: String) extends HttpHeaderRequest with HttpHeaderResponse 

  class NullHeader extends HttpHeader {
    def value = ""
  }
  case object NullHeader extends HttpHeaderField[NullHeader] {
    override def parse(s: String) = Some(new NullHeader)
  }
}
