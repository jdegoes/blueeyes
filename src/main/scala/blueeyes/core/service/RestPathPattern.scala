package blueeyes.core.service

import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scala.util.matching.Regex

import blueeyes.core.http._

private[service] object PathUtils {
  def sanitizePath(s: String) = ("/" + s + "/").replaceAll("/+", "/")
  
  def toPathElements(path: String): List[String] = {
    sanitizePath(path).split("/").toList.map(_.trim).filter(_.length > 0)
  }
}

// TODO: Must change to URI => Map[Symbol, String]
sealed trait RestPathPattern extends PartialFunction[String, Map[Symbol, String]] { self =>
  import RestPathPatternParsers._
  
  def parser: Parser[Map[Symbol, String]]
  
  private def applyParser = startOfString ~> parser
  
  def isDefinedAt(s: String): Boolean = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true
    
    case _ => false
  }
  
  def apply(s: String): Map[Symbol, String] = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result
    
    case Failure(msg, _) => parseFailure(msg, s)
    
    case Error(msg, _) => parseError(msg, s)
  }
  
  def / (symbol: Symbol): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ RestPathPatternParsers.SymbolPathPattern(symbol).parser ^^ sequence
    
    override def toString = self.toString + "/" + symbol.toString
  }
  
  def / (regex: Regex, groupNames: List[String]): RestPathPattern = new RestPathPattern {
    private val regexPattern = RestPathPatternParsers.RegexPathPattern(regex, groupNames).parser
    
    val parser = self.parser ~ slashParser ~ regexPattern ^^ sequence
    
    override def toString = self.toString + "/" + regexPattern
  }
  
  def / (string: String): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ RestPathPatternParsers.parse(string).parser ^^ sequence
    
    override def toString = self.toString + "/" + string
  }
  
  def / (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ that.parser ^^ sequence
    
    override def toString = self.toString + "/" + that.toString
  }
  
  def | (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser | that.parser
    
    override def toString = self.toString + " | " + that.toString
  }
  
  def || (that: => RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser | that.parser
    
    override def toString = self.toString + " || " + that.toString
  }
  
  def ~ (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ that.parser ^^ {
      case m1 ~ m2 => m1 ++ m2
    }
    
    override def toString = self.toString + that.toString
  }
  
  def $: RestPathPattern = new RestPathPattern {
    val parser = self.parser <~ endOfString
  }
  
  def shift[T](r: HttpRequest[T]): HttpRequest[T] = {
    parser(new CharSequenceReader(r.subpath)) match {
      case Success(result, next) => 
        val remainingPath = next.source.subSequence(next.offset, next.source.length).toString
        
        r.withSubpath(remainingPath)
      
      case Failure(msg, _) => parseFailure(msg, r.subpath)
      
      case Error(msg, _) => parseError(msg, r.subpath)
    }
  }
  
  private def sequence(a: ~[~[Map[Symbol, String], Map[Symbol, String]], Map[Symbol, String]]) = a match {
    case m1 ~ m2 ~ m3 => m1 ++ m2 ++ m3
  }
  
  private def slashParser = RestPathPatternParsers.SlashPathPattern.parser
  
  private def parseFailure(msg: String, s: String) = {
    error("The pattern " + this.toString + " does not match " + s + ": " + msg)
  }
  private def parseError(msg: String, s: String) = {
    error("There was an error parsing \"" + s + "\" with pattern \"" + this.toString + "\": " + msg)
  }
}
object RestPathPattern extends RegexParsers {
  def Root = RestPathPatternParsers.RootPathPattern
  
  def apply(string: String): RestPathPattern = RestPathPatternParsers.parse(string)
}
object RestPathPatternParsers extends RegexParsers {
  override def skipWhitespace = false
  
  def validUrlFrag:     Parser[String] = """[a-zA-Z0-9$\-_.+!*'()]+""".r
  def validUrlIdent:    Parser[String] = """[a-zA-Z0-9\-_]+""".r
  def validSymbolName:  Parser[String] = """[a-zA-Z_][a-zA-Z_0-9]*""".r
  def pathSeparator:    Parser[String] = "/"
  def startOfString:    Parser[String] = """^""".r
  def endOfString:      Parser[String] = """$""".r
  
  def symbol:  Parser[Symbol] = ("'" ~> validSymbolName) ^^ (s => Symbol(s))
  def literal: Parser[String] = validUrlFrag
  
  def pathElement: Parser[RestPathPattern] = (symbol ^^ (s => SymbolPathPattern(s))) | (literal ^^ (s => LiteralPathPattern(s))) | (pathSeparator ^^ (s => LiteralPathPattern(s)))
  
  def restPathPatternParser: Parser[List[RestPathPattern]] = startOfString ~> (pathElement*) <~ endOfString
  
  def parse(s: String): RestPathPattern = {
    val elements = restPathPatternParser(new CharSequenceReader(s)) match {
      case Success(result, _) => result
      
      case Failure(msg, _) => error("The path specification " + s + " has a syntax error: " + msg)
      
      case Error(msg, _) => error("There was an error parsing \"" + s + "\": " + msg)
    }
    
    CompositePathPattern(elements)
  }
  
  case class LiteralPathPattern(text: String) extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = literal(text) ^^^ Map()
    
    override def toString = text
  }  
  object SlashPathPattern extends LiteralPathPattern("/")
  object RootPathPattern extends LiteralPathPattern("")
  object EmptyPathPattern extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = """^$""".r ^^^ Map()
    
    override def toString = "^$"
  }
  case class SymbolPathPattern(symbol: Symbol) extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = validUrlIdent ^^ (s => Map(symbol -> s))
    
    override def toString = symbol.toString
  }  
  case class RegexPathPattern(regex: Regex, groupNames: List[String]) extends RestPathPattern {
    val parser = new Parser[Map[Symbol, String]] {
      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
        //(regex findFirstMatchIn (source.subSequence(start, source.length))) match {
        (regex findPrefixMatchOf (source.subSequence(start, source.length))) match {
          case Some(matched) => 
            Success(Map(groupNames.map(name => Symbol(name) -> matched.group(name)): _*), in.drop(start + matched.end - offset))
          case None =>
            Failure("string matching regex `"+regex+"' expected but `"+in.first+"' found", in.drop(start - offset))
        }
      }
    }
    
    override def toString = regex.pattern.toString
  }
  case class CompositePathPattern(elements: List[RestPathPattern]) extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = elements match {
      case head :: tail => tail.foldLeft[Parser[Map[Symbol, String]]](elements.head.parser) { (composite, e) =>
        (composite ~ e.parser) ^^ (pair => pair._1 ++ pair._2)
      }
      
      case Nil => RestPathPattern.Root.parser
    }
    
    override def toString = elements.mkString("")
  }
}
trait RestPathPatternImplicits {
  implicit def stringToRestPathPathPattern(s: String): RestPathPattern = RestPathPatternParsers.parse(s)
  
  implicit def symbolToRestPathPathPattern(s: Symbol): RestPathPattern = RestPathPatternParsers.SymbolPathPattern(s)
  
  /** Example: new Regex("""([a-z]+)""", "id") ~ List('id) 
   *  In use example: ads / foo / new Regex("""([a-z]+)""", "id") ~ List('id)
   *
   *  A note about groups for scala Regex:  
   *    Scala Regex is based off of Java Pattern, in which groups are not given
   *    names, only numbers based on the order in which they appear.  Thus,
   *    the labels one gives groups in the Scala Regex are assigned based on
   *    ordering from the java Pattern.  As such, in the following example,
   *      New Regex("""(a|z)(b+)""", "id1", "id2")
   *    id1 referss to the capture group a|z, whereas id2 refers to b+. 
   */
  implicit def regexToRestPathPatternBuilder(regex: Regex) = new {
    def ~ (names: List[Symbol]) = RestPathPatternParsers.RegexPathPattern(regex, names.map(_.name))
  }
}
object RestPathPatternImplicits extends RestPathPatternImplicits
