package blueeyes.parsers

import scala.util.parsing.combinator._
import scala.util.matching.Regex

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter

object W3ExtendedLogAST {
  private val DTFormat = DateTimeFormat.forPattern("dd-MM-yyyy HH:mm:ss")

  sealed trait Directive
  case class VersionDirective  (major: Int, minor: Int) extends Directive{
    override def toString = "#Version: " + major + "." + minor
  }
  case class FieldsDirective   (identifiers: List[FieldIdentifier]) extends Directive{
    override def toString = "#Fields: " + identifiers.mkString(" ")
  }
  case class SoftwareDirective (value: String)  extends Directive{
    override def toString = "#Software: " + value
  }
  case class StartDateDirective(date: DateTime) extends Directive{
    override def toString = "#Start-Date: " + DTFormat.print(date)
  }
  case class EndDateDirective  (date: DateTime) extends Directive{
    override def toString = "#End-Date: " + DTFormat.print(date)
  }
  case class DateDirective     (date: DateTime) extends Directive{
    override def toString = "#Date: " + DTFormat.print(date)
  }
  case class RemarkDirective   (text: String)   extends Directive{
    override def toString = "#Remark: " + text
  }
  
  sealed trait FieldIdentifier
  sealed trait PredefinedIdentifier extends FieldIdentifier 
  case class HeaderIdentifier(prefix: Prefix, header: String)   extends FieldIdentifier{
    override def toString = prefix.toString + "(" + header + ")"
  }
  case class CustomIdentifier(value: String) extends FieldIdentifier {
    def prefix = AppSpecificPrefix

    override def toString = prefix.toString + "-" + value
  }
  
  sealed trait Prefix{
    def prefix: String

    override def toString = prefix
  }
  case object ClientPrefix          extends Prefix{
    def prefix = "c"
  }
  case object ServerPrefix          extends Prefix{
    def prefix = "s"
  }
  case object RemotePrefix          extends Prefix{
    def prefix = "r"
  }
  case object ClientToServerPrefix  extends Prefix{
    def prefix = "cs"
  }
  case object ServerToClientPrefix  extends Prefix{
    def prefix = "sc"
  }
  case object ServerToRemoteServer  extends Prefix{
    def prefix = "sr"
  }
  case object RemoteServerToServer  extends Prefix{
    def prefix = "rs"
  }
  case object AppSpecificPrefix     extends Prefix{
    def prefix = "x"
  }
    
  sealed trait PredefinedIdentifierNoPrefix extends PredefinedIdentifier{
    def identifier: String

    override def toString = identifier
  }
  case object DateIdentifier      extends PredefinedIdentifierNoPrefix{
    def identifier = "date"
  }
  case object TimeIdentifier      extends PredefinedIdentifierNoPrefix{
    def identifier = "time"
  }
  case object TimeTakenIdentifier extends PredefinedIdentifierNoPrefix{
    def identifier = "time-taken"
  }
  case object BytesIdentifier     extends PredefinedIdentifierNoPrefix{
    def identifier = "bytes"
  }
  case object CachedIdentifier    extends PredefinedIdentifierNoPrefix{
    def identifier = "cached"
  }
  
  sealed trait PredefinedIdentifierPrefix extends PredefinedIdentifier {
    def prefix: Prefix
    def identifier: String

    override def toString = prefix.toString + "-" + identifier
  }
  case class IpIdentifier       (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "ip"
  }
  case class DnsNameIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "dns"
  }
  case class StatusIdentifier   (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "status"
  }
  case class CommentIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "comment"
  }
  case class MethodIdentifier   (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "method"
  }
  case class UriIdentifier      (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "uri"
  }
  case class UriStemIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "uri-stem"
  }
  case class UriQueryIdentifier (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "uri-query"
  }
  case class ContentIdentifier   (prefix: Prefix) extends PredefinedIdentifierPrefix{
    def identifier = "content"
  }
  
  case class LogEntry(fields: List[Field])
  
  sealed trait Field
  case class IntegerField(value: Int)   extends Field
  case class FixedField(value: Double)  extends Field
  case class UriField(value: String)    extends Field
  case class DateField(value: DateTime) extends Field
  case class TimeField(value: DateTime) extends Field
  case class StringField(value: String) extends Field
}


trait W3ExtendedLogGrammar extends JavaTokenParsers {
  import W3ExtendedLogAST._
  
  override def skipWhitespace = true
  
  private[this] val DateTimeFormats = List[(Regex, DateTimeFormatter)](
    ("\\d{8}\\s\\d{6}".r                                           , DateTimeFormat.forPattern("yyyyMMdd HHmmss")),
    ("\\d{14}".r                                                   , DateTimeFormat.forPattern("yyyyMMddHHmmss")),
    ("\\d{8}\\s\\d{4}".r                                           , DateTimeFormat.forPattern("yyyyMMdd HHmm")),
    ("\\d{12}".r                                                   , DateTimeFormat.forPattern("yyyyMMddHHmm")),
    ("\\d{8}".r                                                    , DateTimeFormat.forPattern("yyyyMMdd")),
    ("\\d{1,2}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r         , DateTimeFormat.forPattern("dd-MM-yyyy HH:mm:ss")),
    ("\\d{1,2}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}".r                , DateTimeFormat.forPattern("dd-MM-yyyy HH:mm")),
    ("\\d{1,2}-\\d{1,2}-\\d{4}".r                                  , DateTimeFormat.forPattern("dd-MM-yyyy")),
    ("\\d{4}-\\d{1,2}-\\d{1,2}\\s\\d{1,2}:\\d{2}:\\d{2}".r         , DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")),
    ("\\d{4}-\\d{1,2}-\\d{1,2}\\s\\d{1,2}:\\d{2}".r                , DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")),
    ("\\d{4}-\\d{1,2}-\\d{1,2}".r                                  , DateTimeFormat.forPattern("yyyy-MM-dd")),
    ("\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r         , DateTimeFormat.forPattern("MM/dd/yyyy HH:mm:ss")),
    ("\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}".r                , DateTimeFormat.forPattern("MM/dd/yyyy HH:mm")),
    ("\\d{1,2}/\\d{1,2}/\\d{4}".r                                  , DateTimeFormat.forPattern("MM/dd/yyyy")),
    ("\\d{4}/\\d{1,2}/\\d{1,2}\\s\\d{1,2}:\\d{2}:\\d{2}".r         , DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss")),
    ("\\d{4}/\\d{1,2}/\\d{1,2}\\s\\d{1,2}:\\d{2}".r                , DateTimeFormat.forPattern("yyyy/MM/dd HH:mm")),
    ("\\d{4}/\\d{1,2}/\\d{1,2}".r                                  , DateTimeFormat.forPattern("yyyy/MM/dd")),
    ("\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r  , DateTimeFormat.forPattern("dd MMM yyyy HH:mm:ss")),
    ("\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}\\s\\d{1,2}:\\d{2}".r         , DateTimeFormat.forPattern("dd MMM yyyy HH:mm")),
    ("\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}".r                           , DateTimeFormat.forPattern("dd MMM yyyy")),
    ("\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r , DateTimeFormat.forPattern("dd MMMM yyyy HH:mm:ss")),
    ("\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}\\s\\d{1,2}:\\d{2}".r        , DateTimeFormat.forPattern("dd MMMM yyyy HH:mm")),
    ("\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}".r                          , DateTimeFormat.forPattern("dd MMMM yyyy")),
    ("\\d{2}-[a-zA-Z]{3}-\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r        , DateTimeFormat.forPattern("dd-MMM-yyyy HH:mm:ss"))
  )

  lazy val dateTimeParser = DateTimeFormats.foldLeft[Parser[DateTime]](failure("No match")) { (parsers, tuple) =>
    val (pattern, formatter) = tuple
    parsers | (pattern ^^ { (string: String) =>
      formatter.parseDateTime(string)
    })
  }

  lazy val newline = """(?:(\r)?\n)|$""".r
  
  lazy val anythingButNewline: Parser[String] = """(\S[^\r\n]*)""".r
  
  lazy val identifier: Parser[String] = """([a-zA-Z0-9\-_]+)""".r
    
  lazy val directive = "#" ~> (versionDirective | fieldsDirective | softwareDirective | dateDirective | startDateDirective | endDateDirective | remarkDirective) <~ (newline?)

  lazy val directives = directive*
  
  lazy val versionDirective = "Version" ~> ":" ~> ((wholeNumber <~ ".") ~ wholeNumber) ^^ {
    case major ~ minor => VersionDirective(major.toInt, minor.toInt)
  }
  
  lazy val fieldsDirective = "Fields" ~> ":" ~> (fieldIdentifier*) ^^ (list => FieldsDirective(list))
  
  lazy val softwareDirective = "Software" ~> ":" ~> anythingButNewline ^^ (s => SoftwareDirective(s))
  
  lazy val startDateDirective = "Start-Date" ~> ":" ~> dateTimeParser ^^ (dt => StartDateDirective(dt))
  
  lazy val endDateDirective = "End-Date" ~> ":" ~> dateTimeParser ^^ (dt => EndDateDirective(dt))
  
  lazy val dateDirective = "Date" ~> ":" ~> dateTimeParser ^^ (dt => DateDirective(dt))
  
  lazy val remarkDirective = "Remark" ~> ":" ~> anythingButNewline ^^ (s => RemarkDirective(s))
  
  lazy val fieldIdentifier = simpleIdentifier | prefixedIdentifier | headerIdentifier | customIdentifier
  
  lazy val simpleIdentifier: Parser[PredefinedIdentifierNoPrefix] = {
    ("date"       ^^^ DateIdentifier)       |
    ("time-taken" ^^^ TimeTakenIdentifier)  |
    ("time"       ^^^ TimeIdentifier)       |
    ("bytes"      ^^^ BytesIdentifier)      |
    ("cached"     ^^^ CachedIdentifier)
  }
  
  lazy val prefixedIdentifier: Parser[PredefinedIdentifierPrefix] = {
    (prefix <~ "-").flatMap { prefix =>
      ("ip"         ^^^ IpIdentifier(prefix))      |
      ("dns"        ^^^ DnsNameIdentifier(prefix)) |
      ("status"     ^^^ StatusIdentifier(prefix))  |
      ("comment"    ^^^ CommentIdentifier(prefix)) |
      ("method"     ^^^ MethodIdentifier(prefix))  |
      ("uri-stem"   ^^^ UriStemIdentifier(prefix)) |
      ("uri-query"  ^^^ UriQueryIdentifier(prefix)) |
      ("uri"        ^^^ UriIdentifier(prefix))     |
      ("content"    ^^^ ContentIdentifier(prefix))
    }
  }

  lazy val headerIdentifier: Parser[HeaderIdentifier] = {
    (prefix <~ "(") ~ (identifier <~ ")") ^^ {
      case prefix ~ header => HeaderIdentifier(prefix, header)
    }
  }
  
  lazy val customIdentifier: Parser[CustomIdentifier] = {
    "x" ~> "-" ~> identifier ^^ (s => CustomIdentifier(s))
  }
  
  lazy val prefix: Parser[Prefix] = {
    ("cs" ^^^ ClientToServerPrefix) |
    ("sc" ^^^ ServerToClientPrefix) |
    ("sr" ^^^ ServerToRemoteServer) |
    ("rs" ^^^ RemoteServerToServer) |
    ("c"  ^^^ ClientPrefix)         |
    ("s"  ^^^ ServerPrefix)         |
    ("r"  ^^^ RemotePrefix)
  }
}

object W3ExtendedLogGrammar extends W3ExtendedLogGrammar
