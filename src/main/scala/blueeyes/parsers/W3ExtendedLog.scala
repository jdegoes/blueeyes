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
  
  private[this] val dateTimeParser1 = DateTimeFormat.forPattern("YYYY-MM-DD HH:MM")
  private[this] val dateTimeParser2 = DateTimeFormat.forPattern("YYYY-MM-DD HH:MM:SS")
  private[this] val dateTimeParser3 = DateTimeFormat.forPattern("YYYY-MM-DD HH:MM:SS.S")
  
  private[this] def toPF[A, B](f: A => B): PartialFunction[A, B] = new PartialFunction[A, B] {
    def isDefinedAt(a: A): Boolean = try { f(a); true } catch { case _ => false }
    
    def apply(a: A): B = f(a)
  }
  
  private[this] val parseDateTime1 = toPF((s: String) => dateTimeParser1.parseDateTime(s))
  private[this] val parseDateTime2 = toPF((s: String) => dateTimeParser2.parseDateTime(s))
  private[this] val parseDateTime3 = toPF((s: String) => dateTimeParser3.parseDateTime(s))
  
  val parseDateTime = parseDateTime3.orElse(parseDateTime2).orElse(parseDateTime1)
  
  lazy val newline = """(?:(\r)?\n)|$""".r
  
  lazy val anythingButNewline: Parser[String] = """(\S[^\r\n]*)""".r
  
  lazy val identifier: Parser[String] = """([a-zA-Z0-9\-_]+)""".r
    
  lazy val directive = "#" ~> (versionDirective | fieldsDirective | softwareDirective | endDateDirective) <~ newline
  
  lazy val versionDirective = "Version" ~> ":" ~> ((wholeNumber <~ ".") ~ wholeNumber) ^^ {
    case major ~ minor => VersionDirective(major.toInt, minor.toInt)
  }
  
  lazy val fieldsDirective = "Fields" ~> ":" ~> (fieldIdentifier*) ^^ (list => FieldsDirective(list))
  
  lazy val softwareDirective = "Software" ~> ":" ~> anythingButNewline ^^ (s => SoftwareDirective(s))
  
  lazy val startDateDirective = "Start-Date" ~> ":" ~> anythingButNewline ^^ (s =>
    StartDateDirective(parseDateTime(s))
  )
  
  lazy val endDateDirective = "End-Date" ~> ":" ~> anythingButNewline ^^ (s =>
    EndDateDirective(parseDateTime(s))
  )
  
  lazy val dateDirective = "Date" ~> ":" ~> anythingButNewline ^^ (s =>
    DateDirective(parseDateTime(s))
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

}
