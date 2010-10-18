package blueeyes.core.service

import util.matching.Regex

sealed trait RestPathPattern extends PartialFunction[String, Map[Symbol, String]] { self =>
  def elementPatterns: List[PathElement]
  
  def isDefinedAt(s: String) = {
    var elementStrings = s.split('/').toList
    
    elementPatterns.length == elementStrings.length && elementPatterns.zip(elementStrings).takeWhile(t => t._1.isDefinedAt(t._2)).length == elementPatterns.length
  }
  
  def apply(s: String) = {
    var elementStrings = s.split('/').toList
    
    Map(elementPatterns.zip(elementStrings).flatMap(t => t._1(t._2)): _*)
  }
  
  def ++ (that: RestPathPattern) = new RestPathPattern {
    def elementPatterns = self.elementPatterns ++ that.elementPatterns
  }
  def / (tailElement: PathElement) = new RestPathPattern {
    def elementPatterns = self.elementPatterns :+ tailElement
  }

  override def toString = elementPatterns.mkString("/")
}

object RestPathPattern {
  def Root = new RestPathPattern { def elementPatterns = Nil }

  implicit def stringToRestPathPattern(element: String) = Root / StringElement(element)
  implicit def symbolToRestPathPattern(element: Symbol) = Root / SymbolElement(element)
}
sealed trait PathElement extends PartialFunction[String, List[(Symbol, String)]]
case class StringElement(element: String) extends PathElement{
  def isDefinedAt(s: String) = element == s

  def apply(s: String) = Nil

  override def toString = element
}
case class SymbolElement(element: Symbol) extends PathElement{
  def isDefinedAt(s: String) = true
  
  def apply(s: String) = (element -> s) :: Nil

  override def toString = "'" + element.name
}
case class RegexElement(pattern: Regex, names: List[String]) {
  def isDefinedAt(s: String) = s match {
    case pattern(matches) => true
    case _ => false
  }
  
  def apply(s: String) = {
    val matches: List[String] = pattern.unapplySeq(s).get
    
    matches.foldLeft[(List[(Symbol, String)], Int)]((Nil, 0)) { (state, captured) =>
      val list  = state._1
      val index = state._2
      val name  = names(index)
      
      ((Symbol(name), captured) :: list, index + 1)
    }._1
  }

  override def toString = "pattern=%s; names=%s".format(pattern.toString, names.mkString(", "))
}

object PathElement{
  implicit def stringToStringElement(element: String) = StringElement(element)
  implicit def symbolToSymbolElement(element: Symbol) = SymbolElement(element)
}