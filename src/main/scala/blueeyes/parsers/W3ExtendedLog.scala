package blueeyes.parsers

import scala.util.parsing.combinator._
import scala.util.matching.Regex

import org.joda.time.DateTime
import org.joda.time.DateTimeZone
import org.joda.time.format.DateTimeFormat
import org.joda.time.format.DateTimeFormatter

object W3ExtendedLogAST {
  case class VersionDirective  (major: Int, minor: Int)  
  case class FieldsDirective   (identifiers: List[FieldIdentifier])  
  case class SoftwareDirective (value: String)  
  case class StartDateDirective(date: DateTime)
  case class EndDateDirective  (date: DateTime)
  case class DateDirective     (date: DateTime)
  case class RemarkDirective   (text: String)
  
  sealed trait FieldIdentifier
  sealed trait PredefinedIdentifier extends FieldIdentifier 
  case class HeaderIdentifier(prefix: Prefix, header: String)   extends FieldIdentifier
  case class CustomIdentifier(value: String) extends FieldIdentifier {
    def prefix = AppSpecificPrefix
  }
  
  sealed trait Prefix
  case object ClientPrefix          extends Prefix
  case object ServerPrefix          extends Prefix
  case object RemotePrefix          extends Prefix
  case object ClientToServerPrefix  extends Prefix
  case object ServerToClientPrefix  extends Prefix
  case object ServerToRemoteServer  extends Prefix
  case object RemoteServerToServer  extends Prefix
  case object AppSpecificPrefix     extends Prefix
    
  sealed trait PredefinedIdentifierNoPrefix extends PredefinedIdentifier
  case object DateIdentifier      extends PredefinedIdentifierNoPrefix
  case object TimeIdentifier      extends PredefinedIdentifierNoPrefix
  case object TimeTakenIdentifier extends PredefinedIdentifierNoPrefix
  case object BytesIdentifier     extends PredefinedIdentifierNoPrefix
  case object CachedIdentifier    extends PredefinedIdentifierNoPrefix
  
  sealed trait PredefinedIdentifierPrefix extends PredefinedIdentifier {
    def prefix: Prefix
  }
  case class IpIdentifier       (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class DnsNameIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class StatusIdentifier   (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class CommentIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class MethodIdentifier   (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class UriIdentifier      (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class UriStemIdentifier  (prefix: Prefix) extends PredefinedIdentifierPrefix
  case class UriQueryIdentifier (prefix: Prefix) extends PredefinedIdentifierPrefix
  
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
  
  private[this] val DateTimeFormats = Map[Regex, DateTimeFormatter](
    "\\d{8}".r                                                    -> DateTimeFormat.forPattern("yyyyMMdd"),
    "\\d{1,2}-\\d{1,2}-\\d{4}".r                                  -> DateTimeFormat.forPattern("dd-MM-yyyy"),
    "\\d{4}-\\d{1,2}-\\d{1,2}".r                                  -> DateTimeFormat.forPattern("yyyy-MM-dd"),
    "\\d{1,2}/\\d{1,2}/\\d{4}".r                                  -> DateTimeFormat.forPattern("MM/dd/yyyy"),
    "\\d{4}/\\d{1,2}/\\d{1,2}".r                                  -> DateTimeFormat.forPattern("yyyy/MM/dd"),
    "\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}".r                           -> DateTimeFormat.forPattern("dd MMM yyyy"),
    "\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}".r                          -> DateTimeFormat.forPattern("dd MMMM yyyy"),
    "\\d{12}".r                                                   -> DateTimeFormat.forPattern("yyyyMMddHHmm"),
    "\\d{8}\\s\\d{4}".r                                           -> DateTimeFormat.forPattern("yyyyMMdd HHmm"),
    "\\d{1,2}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}".r                -> DateTimeFormat.forPattern("dd-MM-yyyy HH:mm"),
    "\\d{4}-\\d{1,2}-\\d{1,2}\\s\\d{1,2}:\\d{2}".r                -> DateTimeFormat.forPattern("yyyy-MM-dd HH:mm"),
    "\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}".r                -> DateTimeFormat.forPattern("MM/dd/yyyy HH:mm"),
    "\\d{4}/\\d{1,2}/\\d{1,2}\\s\\d{1,2}:\\d{2}".r                -> DateTimeFormat.forPattern("yyyy/MM/dd HH:mm"),
    "\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}\\s\\d{1,2}:\\d{2}".r         -> DateTimeFormat.forPattern("dd MMM yyyy HH:mm"),
    "\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}\\s\\d{1,2}:\\d{2}".r        -> DateTimeFormat.forPattern("dd MMMM yyyy HH:mm"),
    "\\d{14}".r                                                   -> DateTimeFormat.forPattern("yyyyMMddHHmmss"),
    "\\d{8}\\s\\d{6}".r                                           -> DateTimeFormat.forPattern("yyyyMMdd HHmmss"),
    "\\d{1,2}-\\d{1,2}-\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r         -> DateTimeFormat.forPattern("dd-MM-yyyy HH:mm:ss"),
    "\\d{4}-\\d{1,2}-\\d{1,2}\\s\\d{1,2}:\\d{2}:\\d{2}".r         -> DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss"),
    "\\d{2}-[a-zA-Z]{3}-\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r        -> DateTimeFormat.forPattern("dd-MMM-yyyy HH:mm:ss"),
    "\\d{1,2}/\\d{1,2}/\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r         -> DateTimeFormat.forPattern("MM/dd/yyyy HH:mm:ss"),
    "\\d{4}/\\d{1,2}/\\d{1,2}\\s\\d{1,2}:\\d{2}:\\d{2}".r         -> DateTimeFormat.forPattern("yyyy/MM/dd HH:mm:ss"),
    "\\d{1,2}\\s[a-zA-Z]{3}\\s\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r  -> DateTimeFormat.forPattern("dd MMM yyyy HH:mm:ss"),
    "\\d{1,2}\\s[a-zA-Z]{4,}\\s\\d{4}\\s\\d{1,2}:\\d{2}:\\d{2}".r -> DateTimeFormat.forPattern("dd MMMM yyyy HH:mm:ss")
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
    
  lazy val directive = "#" ~> (versionDirective | fieldsDirective | softwareDirective | dateDirective | startDateDirective | endDateDirective) <~ newline
  
  lazy val versionDirective = "Version" ~> ":" ~> ((wholeNumber <~ ".") ~ wholeNumber) ^^ {
    case major ~ minor => VersionDirective(major.toInt, minor.toInt)
  }
  
  lazy val fieldsDirective = "Fields" ~> ":" ~> (fieldIdentifier*) ^^ (list => FieldsDirective(list))
  
  lazy val softwareDirective = "Software" ~> ":" ~> anythingButNewline ^^ (s => SoftwareDirective(s))
  
  lazy val startDateDirective = "Start-Date" ~> ":" ~> dateTimeParser ^^ (dt =>
    StartDateDirective(dt)
  )
  
  lazy val endDateDirective = "End-Date" ~> ":" ~> dateTimeParser ^^ (dt =>
    EndDateDirective(dt)
  )
  
  lazy val dateDirective = "Date" ~> ":" ~> dateTimeParser ^^ (dt =>
    DateDirective(dt)
  )
  
  lazy val remarkDirective = "Remark" ~> ":" ~> anythingButNewline ^^ (s => RemarkDirective(s))
  
  lazy val fieldIdentifier = customIdentifier | prefixedIdentifier | simpleIdentifier | headerIdentifier
  
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
      ("uri"        ^^^ UriIdentifier(prefix))     |
      ("uri-stem"   ^^^ UriStemIdentifier(prefix)) |
      ("uri-query"  ^^^ UriQueryIdentifier(prefix))
    }
  }
  
  lazy val headerIdentifier: Parser[HeaderIdentifier] = {
    (prefix <~ "(") ~ (identifier <~ ")") ^^ {
      case prefix ~ header => HeaderIdentifier(prefix, header)
    }
  }
  
  lazy val customIdentifier: Parser[CustomIdentifier] = {
    "x" ~> identifier ^^ (s => CustomIdentifier(s))
  }
  
  lazy val prefix: Parser[Prefix] = {
    ("cs" ^^^ ClientToServerPrefix) |
    ("sc" ^^^ ServerToClientPrefix) |
    ("sr" ^^^ ServerToRemoteServer) |
    ("rs" ^^^ RemoteServerToServer) |
    ("c"  ^^^ ClientPrefix)         |
    ("s"  ^^^ ServerPrefix)         |
    ("x"  ^^^ AppSpecificPrefix)
  }
  
  //lazy val fields = 
  
  
}
object W3ExtendedLogGrammar extends W3ExtendedLogGrammar

object W3ExtendedLog {
  import W3ExtendedLogAST._
  import W3ExtendedLogGrammar._

  /*
  import blueeyes.parsers.W3ExtendedLogGrammar._
  import blueeyes.parsers.W3ExtendedLogGrammar._

  import scala.util.parsing.input.CharSequenceReader
  import scala.util.parsing.input.CharSequenceReader

  implicit def stringToCharSequenceReader(s: String) = new CharSequenceReader(s)
  directive("#Version: 1.0")
  directive("#Date: 12-Jan-1996 00:00:00")
  directive("#Fields: time cs-method cs-uri")
  */
}
