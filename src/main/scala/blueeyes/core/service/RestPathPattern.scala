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

sealed trait RestPathPattern extends PartialFunction[String, Map[Symbol, String]] { self =>
  def elementPatterns: List[PathElement]
  
  def isDefinedAt(s: String) = {
    var index = if (s.startsWith("/")) 1 else 0
    var elementStrings = s.substring(index).split('/').toList
    
    elementPatterns.length == elementStrings.length && elementPatterns.zip(elementStrings).takeWhile(t => t._1.isDefinedAt(t._2)).length == elementPatterns.length
  }
  
  def apply(s: String) = {
    var pathElements = PathUtils.toPathElements(s)
    
    Map(elementPatterns.zip(pathElements).flatMap(t => t._1(t._2)): _*)
  }
  
  def / (tailElement: RestPathPattern) = new RestPathPattern {
    def elementPatterns = self.elementPatterns ++ tailElement.elementPatterns
  }
  
  override def toString = PathUtils.sanitizePath(elementPatterns.mkString("/"))
}

sealed trait RestPathPattern2 extends PartialFunction[String, Map[Symbol, String]] { self =>
  import RestPathPatternParsers._
  
  def parser: Parser[Map[Symbol, String]]
  
  def isDefinedAt(s: String): Boolean = parser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true
    
    case _ => false
  }
  
  def apply(s: String): Map[Symbol, String] = parser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result
    
    case _ => error("Cannot parse " + s)
  }
  
  def / (symbol: Symbol): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slashParser ~ RestPathPatternParsers.SymbolPathPattern(symbol).parser ^^ sequence
  }
  
  def / (regex: Regex, groupNames: List[String]): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slashParser ~ RestPathPatternParsers.RegexPathPattern(regex, groupNames).parser ^^ sequence
  }
  
  def / (string: String): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slashParser ~ RestPathPatternParsers.parse(string).parser ^^ sequence
  }
  
  def / (that: RestPathPattern2): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slashParser ~ that.parser ^^ sequence
  }
  
  def | (that: RestPathPattern2): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser | that.parser
  }
  
  def || (that: => RestPathPattern2): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser | that.parser
  }
  
  private def sequence(a: ~[~[Map[Symbol, String], Map[Symbol, String]], Map[Symbol, String]]) = a match {
    case m1 ~ m2 ~ m3 => m1 ++ m2 ++ m3
  }
  
  private def slashParser = RestPathPatternParsers.SlashPathPattern.parser
}
object RestPathPattern2 extends RegexParsers {
  def apply(string: String): RestPathPattern2 = RestPathPatternParsers.parse(string)
}
object RestPathPatternParsers extends RegexParsers {
  lazy val Root: RestPathPattern2 = new RestPathPattern2 {
    def parser: Parser[Map[Symbol, String]] = "" ^^^ Map()
  }

  override def skipWhitespace = false
  
  def validUrlFrag:     Parser[String] = """[a-zA-z0-9$\-_.+!*'()]+""".r
  def validSymbolName:  Parser[String] = """[a-zA-Z_][a-zA-z_0-9]*""".r
  def pathSeparator:    Parser[String] = "/"
  def startOfString:    Parser[String] = """^""".r
  def endOfString:      Parser[String] = """$""".r
  
  def symbol:  Parser[Symbol] = ("'" ~> validSymbolName) ^^ (s => Symbol(s))
  def literal: Parser[String] = validUrlFrag
  
  def pathElement: Parser[RestPathPattern2] = (symbol ^^ (s => SymbolPathPattern(s))) | (literal ^^ (s => LiteralPathPattern(s))) | (pathSeparator ^^ (s => LiteralPathPattern(s)))
  
  def restPathPatternParser: Parser[List[RestPathPattern2]] = startOfString ~> (pathElement*) <~ endOfString
  
  def parse(s: String): RestPathPattern2 = {
    val elements = restPathPatternParser(new CharSequenceReader(s)) match {
      case Success(result, _) => /*println(result);*/ result
      
      case _ => error("The path specification " + s + " has a syntax error")
    }
    
    CompositePathPattern(elements)
  }
  
  case class LiteralPathPattern(text: String) extends RestPathPattern2 {
    val parser: Parser[Map[Symbol, String]] = literal(text) ^^^ Map()
    
    override def toString = text
  }  
  object SlashPathPattern extends LiteralPathPattern("/")
  case class SymbolPathPattern(symbol: Symbol) extends RestPathPattern2 {
    val parser: Parser[Map[Symbol, String]] = validUrlFrag ^^ (s => Map(symbol -> s))
    
    override def toString = symbol.toString
  }  
  case class RegexPathPattern(regex: Regex, groupNames: List[String]) extends RestPathPattern2 {
    val parser = new Parser[Map[Symbol, String]] {
      def apply(in: Input) = {
        val source = in.source
        val offset = in.offset
        val start = handleWhiteSpace(source, offset)
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
  case class CompositePathPattern(elements: List[RestPathPattern2]) extends RestPathPattern2 {
    val parser: Parser[Map[Symbol, String]] = elements match {
      case head :: tail => tail.foldLeft[Parser[Map[Symbol, String]]](elements.head.parser) { (composite, e) =>
        (composite ~ e.parser) ^^ (pair => pair._1 ++ pair._2)
      }
      
      case Nil => Root.parser
    }
    
    override def toString = elements.mkString("")
  }
}
trait RestPathPattern2Implicits {
  implicit def stringToRestPathPathPattern(s: String): RestPathPattern2 = RestPathPatternParsers.parse(s)
  
  implicit def symbolToRestPathPathPattern(s: Symbol): RestPathPattern2 = RestPathPatternParsers.SymbolPathPattern(s)
}
object RestPathPattern2Implicits extends RestPathPattern2Implicits




object RestPathPattern {
  import scala.util.matching.Regex
  
  private val SymbolPattern = """'([\w.\-]+)""".r
  private val PathPattern   = """([\w.\-]+)""".r
  
  def Root = new RestPathPattern { def elementPatterns = Nil }
  
  def apply(string: String): RestPathPattern = new RestPathPattern {
    lazy val elementPatterns = PathUtils.toPathElements(string).map { 
      case SymbolPattern(name) => SymbolElement(Symbol(name))
      case PathPattern(name)   => StringElement(name)
      case _ => error("Unknown rest path pattern: " + string)
    }
  }
}

sealed trait PathElement extends RestPathPattern { self =>
  def elementPatterns = List(self)
}
case class StringElement(element: String) extends PathElement {
  override def isDefinedAt(s: String) = element == s

  override def apply(s: String) = Map()
  
  override def toString = element
}
case class SymbolElement(element: Symbol) extends PathElement {
  override def isDefinedAt(s: String) = true
  
  override def apply(s: String) = Map(element -> s)
  
  override def toString = element.toString
}
case class RegexElement(pattern: Regex, names: List[String]) extends PathElement {
  override def isDefinedAt(s: String) = s match {
    case pattern(matches) => true
    case _ => false
  }
  
  override def apply(s: String) = {
    val matches: List[String] = pattern.unapplySeq(s).get
    
    Map(matches.foldLeft[(List[(Symbol, String)], Int)]((Nil, 0)) { (state, captured) =>
      val list  = state._1
      val index = state._2
      val name  = names(index)
      
      ((Symbol(name), captured) :: list, index + 1)
    }._1: _*)
  }
  
   override def toString = pattern.toString + "~(" + names.mkString(",") + ")"
}

trait RestPathPatternImplicits {
  implicit def stringToRestPathPattern(string: String): RestPathPattern = RestPathPattern(string)
  
  implicit def symbolToRestPathPattern(symbol: Symbol): RestPathPattern = SymbolElement(symbol)
  
  // """(\w+)""" ~ List('adId)
  implicit def regexToRestPathPatternCreator(regex: Regex) = new {
    def ~ (names: List[Symbol]) = RegexElement(regex, names.map(_.name))
  }
}
object RestPathPatternImplicits extends RestPathPatternImplicits