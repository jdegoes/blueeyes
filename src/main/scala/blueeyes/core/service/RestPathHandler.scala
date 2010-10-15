package blueeyes.core.service

import blueeyes.util.Future
import util.matching.Regex

case class RestPathHandler[T](pattern: PartialFunction[String, Map[Symbol, String]], 
                              handler: (Map[Symbol, String], HttpRequest[T]) => Future[HttpResponse[T]]) extends PartialFunction[String, HttpRequest[T] => Future[HttpResponse[T]]] {
 def isDefinedAt(url: String) = pattern.isDefinedAt(url)

 def apply(url: String) = (request: HttpRequest[T]) => {
   val symbols = pattern(url)

   handler(symbols, request)
 }
}

sealed trait RestPathPattern extends PartialFunction[String, Map[Symbol, String]] { self =>
  def elementPatterns: List[PathElement]
  
  def isDefinedAt(s: String) = {
    var elementStrings = s.split('/').toList
    
    elementPatterns.length == elementStrings.length && elementPatterns.zip(elementStrings).takeWhile(t => t._1.isDefinedAt(t._2)).length == 0
  }
  
  def apply(s: String) = {
    var elementStrings = s.split('/').toList
    
    Map(elementPatterns.zip(elementStrings).flatMap(t => t._1(t._2)): _*)
  }
  
  def + (that: RestPathPattern) = new RestPathPattern {
    def elementPatterns = self.elementPatterns ++ that.elementPatterns
  }
}

object RestPath {
  def Root = new RestPathPattern { def elementPatterns = Nil }
}
sealed trait PathElement extends PartialFunction[String, List[(Symbol, String)]]
case class StringElement(element: String) {
  def isDefinedAt(s: String) = element == s
  
  def apply(s: String) = Nil
}
case class SymbolElement(element: Symbol) {
  def isDefinedAt(s: String) = true
  
  def apply(s: String) = (element -> s) :: Nil
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
}


sealed trait RestPathHandlerType