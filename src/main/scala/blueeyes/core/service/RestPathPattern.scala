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
  import RestPathPatternParsers._
  
  def parser: Parser[Map[Symbol, String]]
  
  private def applyParser = startOfString ~> parser
  
  def isDefinedAt(s: String): Boolean = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => true
    
    case _ => false
  }
  
  def apply(s: String): Map[Symbol, String] = applyParser.apply(new CharSequenceReader(s)) match {
    case Success(result, _) => result
    
    case _ => error("Cannot parse " + s)
  }
  
  def / (symbol: Symbol): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ RestPathPatternParsers.SymbolPathPattern(symbol).parser ^^ sequence
  }
  
  def / (regex: Regex, groupNames: List[String]): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ RestPathPatternParsers.RegexPathPattern(regex, groupNames).parser ^^ sequence
  }
  
  def / (string: String): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ RestPathPatternParsers.parse(string).parser ^^ sequence
  }
  
  def / (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ slashParser ~ that.parser ^^ sequence
  }
  
  def | (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser | that.parser
  }
  
  def || (that: => RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser | that.parser
  }
  
  def + (that: RestPathPattern): RestPathPattern = new RestPathPattern {
    val parser = self.parser ~ that.parser ^^ {
      case m1 ~ m2 => m1 ++ m2
    }
  }
  
  def $: RestPathPattern = new RestPathPattern {
    val parser = self.parser <~ endOfString
  }
  
  private def sequence(a: ~[~[Map[Symbol, String], Map[Symbol, String]], Map[Symbol, String]]) = a match {
    case m1 ~ m2 ~ m3 => m1 ++ m2 ++ m3
  }
  
  private def slashParser = RestPathPatternParsers.SlashPathPattern.parser
}
object RestPathPattern extends RegexParsers {
  def Root = RestPathPatternParsers.RootPathPattern
  
  def apply(string: String): RestPathPattern = RestPathPatternParsers.parse(string)
}
object RestPathPatternParsers extends RegexParsers {
  override def skipWhitespace = false
  
  def validUrlFrag:     Parser[String] = """[a-zA-z0-9$\-_.+!*'()]+""".r
  def validSymbolName:  Parser[String] = """[a-zA-Z_][a-zA-z_0-9]*""".r
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
      
      case _ => error("The path specification " + s + " has a syntax error")
    }
    
    println(elements.mkString(""))
    
    CompositePathPattern(elements)
  }
  
  case class LiteralPathPattern(text: String) extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = literal(text) ^^^ Map()
    
    override def toString = text
  }  
  object SlashPathPattern extends LiteralPathPattern("/")
  object RootPathPattern extends LiteralPathPattern("")
  case class SymbolPathPattern(symbol: Symbol) extends RestPathPattern {
    val parser: Parser[Map[Symbol, String]] = validUrlFrag ^^ (s => Map(symbol -> s))
    
    override def toString = symbol.toString
  }  
  case class RegexPathPattern(regex: Regex, groupNames: List[String]) extends RestPathPattern {
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
  
  // """(\w+)""".r ~ List('adId)
  implicit def regexToRestPathPatternBuilder(regex: Regex) = new {
    def ~ (names: List[Symbol]) = RestPathPatternParsers.RegexPathPattern(regex, names.map(_.name))
  }
}
object RestPathPatternImplicits extends RestPathPatternImplicits