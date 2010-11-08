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
    def parser = self.parser ~ slash ~ RestPathPatternParsers.SymbolElement(symbol) ^^ sequence
  }
  
  def / (regex: Regex, groupNames: List[String]): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slash ~ RestPathPatternParsers.RegexElement(regex, groupNames) ^^ sequence
  }
  
  def / (string: String): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slash ~ RestPathPatternParsers.parse(string).parser ^^ sequence
  }
  
  def / (that: RestPathPattern2): RestPathPattern2 = new RestPathPattern2 {
    def parser = self.parser ~ slash ~ that.parser ^^ sequence
  }
  
  private def sequence(a: ~[~[Map[Symbol, String], Map[Symbol, String]], Map[Symbol, String]]) = a match {
    case m1 ~ m2 ~ m3 => m1 ++ m2 ++ m3
  }
  
  private val slash: RestPathPatternParsers.PathElement = RestPathPatternParsers.LiteralElement("/")
}
object RestPathPattern2 extends RegexParsers {
  def apply(string: String): RestPathPattern2 = RestPathPatternParsers.parse(string)
}
object RestPathPatternParsers extends RegexParsers {
  lazy val Root: RestPathPattern2 = RestPathPattern2.apply("/")
  
  def flattenLP(list: List[~[PathElement, PathElement]]): List[PathElement] = list.flatMap(pair => pair._1 :: pair._2 :: Nil)
  def flattenPL(pair: ~[List[PathElement], List[PathElement]]): List[PathElement] = pair._1 ++ pair._2
  
  override def skipWhitespace = false
  
  def specialUrlChar:   Parser[String] = """[;/?:@=&]""".r
  def validUrlFrag:     Parser[String] = """[a-zA-z0-9$\-_.+!*'()]+""".r
  def validSymbolChar:  Parser[String] = """\w""".r
  def symbolStartChar:  Parser[String] = """'""".r
  def pathSeparator:    Parser[String] = """/""".r
  def startOfString:    Parser[String] = """^""".r
  def endOfString:      Parser[String] = """$""".r
  
  def symbol:  Parser[Symbol] = (symbolStartChar ~> validUrlFrag) ^^ (s => Symbol(s))
  def literal: Parser[String] = validUrlFrag
  
  def pathElement: Parser[PathElement] = (symbol ^^ (s => SymbolElement(s))) | (literal ^^ (s => LiteralElement(s)))
  
  def pathElementL: Parser[List[PathElement]] = pathElement ^^ (p => p :: Nil)
  
  def pathSep: Parser[PathElement] = "/" ^^ (s => LiteralElement(s))
  
  def pathSepL: Parser[List[PathElement]] = pathSep ^^ (p => p :: Nil)
  
  def pathSepOpt: Parser[List[PathElement]] = (pathSep?) ^^ (o => o.toList)
  
  def pathMiddle: Parser[List[PathElement]] = ((pathSep ~ pathElement)*) ^^ flattenLP
  
  //def pathPattern = (((((pathSepOpt ~ pathElementL) ^^ flattenPL) ~ pathMiddle) ^^ flattenPL) ~ pathSepOpt) ^^ flattenPL
  def pathPattern = (pathSepOpt ~ pathElementL ~ pathMiddle ~ pathSepOpt ^^ {
    case leadingSlash ~ first ~ middle ~ trailingSlash => leadingSlash ++ first ++ middle ++ trailingSlash
  }) | pathSepL
  
  def fullPathPattern: Parser[List[PathElement]] = startOfString ~> pathPattern <~ endOfString
  
  def parse(s: String): RestPathPattern2 = {
    val elements = fullPathPattern(new CharSequenceReader(s)) match {
      case Success(result, _) => println(result); result
      
      case _ => error("The path specification " + s + " has a syntax error")
    }
    
    CompositeRestPathPattern(elements)
  }  

  case class CompositeRestPathPattern(elements: List[PathElement]) extends RestPathPattern2 { self =>
    val parser: Parser[Map[Symbol, String]] = CompositeElement(elements)

    
    
  }
  
  sealed trait PathElement extends Parser[Map[Symbol, String]]
  
  case class LiteralElement(literal: String) extends PathElement {
    val parser: Parser[Map[Symbol, String]] = literal ^^^ Map()

    def apply(in: Input) = parser.apply(in)
    
    override def toString = literal
  }  
  case class SymbolElement(symbol: Symbol) extends PathElement {
    val parser: Parser[Map[Symbol, String]] = validUrlFrag ^^ (s => Map(symbol -> s))

    override def apply(in: Input) = parser.apply(in)
    
    override def toString = symbol.toString
  }  
  case class RegexElement(regex: Regex, groupNames: List[String]) extends PathElement {
    override def apply(in: Input) = {
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
    
    override def toString = regex.pattern.toString
  }
  case class CompositeElement(elements: List[PathElement]) extends PathElement {
    val parser: Parser[Map[Symbol, String]] = elements.tail.foldLeft[Parser[Map[Symbol, String]]](elements.head) { (composite, e) =>
      (composite ~ e) ^^ (pair => pair._1 ++ pair._2)
    }
    override def apply(in: Input) = parser.apply(in)
    
    override def toString = elements.mkString("")
  }
}
trait RestPathPattern2Implicits {
  implicit def stringToRestPathPattern(s: String): RestPathPattern2 = RestPathPattern2.apply(s)
  
  implicit def symbolToRestPathPattern(s: Symbol): RestPathPattern2 = RestPathPatternParsers.CompositeRestPathPattern(RestPathPatternParsers.SymbolElement(s) :: Nil)
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