package blueeyes.core.service

import util.matching.Regex

sealed trait RestPathPattern extends PartialFunction[String, Map[Symbol, String]] { self =>
  def elementPatterns: List[PathElement]
  
  def isDefinedAt(s: String) = {
    var elementStrings = s.split('/').toList
    
    elementPatterns.length == elementStrings.length && elementPatterns.zip(elementStrings).takeWhile(t => t._1.isDefinedAt(t._2)).length == elementPatterns.length
  }
  
  def apply(s: String) = {
    var pathElements = RestPathPattern.toPathElements(s)
    
    Map(elementPatterns.zip(pathElements).flatMap(t => t._1(t._2)): _*)
  }
  
  def / (tailElement: RestPathPattern) = new RestPathPattern {
    def elementPatterns = self.elementPatterns ++ tailElement.elementPatterns
  }
}

object RestPathPattern {
  import scala.util.matching.Regex
  
  private val SymbolPattern = """'([\w.\-]+)""".r
  private val PathPattern   = """([\w.\-]+)""".r
  
  def Root = new RestPathPattern { def elementPatterns = Nil }
  
  def toPathElements(path: String): List[String] = {
    def sanitizePath(s: String) = ("/" + path + "/").replaceAll("/+", "/")
    
    sanitizePath(path).split("/").toList.map(_.trim).filter(_.length > 0)
  }
  
  def apply(string: String): RestPathPattern = new RestPathPattern {
    lazy val elementPatterns = toPathElements(string).map { 
      case SymbolPattern(name) => SymbolElement(Symbol(name))
      case PathPattern(name)   => StringElement(name)
    }
  }
}
sealed trait PathElement extends RestPathPattern { self =>
  def elementPatterns = List(self)
}
case class StringElement(element: String) extends PathElement {
  override def isDefinedAt(s: String) = element == s

  override def apply(s: String) = Map()
}
case class SymbolElement(element: Symbol) extends PathElement {
  override def isDefinedAt(s: String) = true
  
  override def apply(s: String) = Map(element -> s)
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