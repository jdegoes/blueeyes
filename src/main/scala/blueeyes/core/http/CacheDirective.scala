package blueeyes.core.http

import scala.util.matching.Regex
import blueeyes.util.ProductPrefixUnmangler

sealed trait CacheDirective extends ProductPrefixUnmangler {

  def name: String = unmangledName
  def delta: Option[HttpNumber] = None
  def fieldNames: Option[String] = None
  def value = (name.toList ++ delta.toList ++ fieldNames.map("\"" + _ + "\"").toList).mkString("=")
}

object CacheDirectives {

  def parseCacheDirectives(inString: String): Array[CacheDirective] ={
    def fieldRegex = new Regex("""([a-z\-]+)=("[a-zA-Z\-]+")""")
    def noFieldRegex = new Regex("""([a-z\-]+)(=(\d)+)?""" )
    def fieldDirectives: Array[CacheDirective] = (fieldRegex.findAllIn(inString)).toArray.map(_.trim.split("=")).map( _ match {
      case Array("private", any) => `private`(Some(any))
      case Array("no-cache", any) => `no-cache`(Some(any))
      case Array("private") => `private`(None)
      case Array("no-cache") => `no-cache`(None)
      case default => NullDirective("null")
    })

    def noFieldDirectives: Array[CacheDirective] = (noFieldRegex.findAllIn(inString)).toArray.map(_.trim.split("=")).map( _ match {
      case Array("no-cache") => `no-cache` 
      case Array("no-store") => `no-store` 
      case Array("max-age", delta) => `max-age`(HttpNumbers.parseHttpNumbers(delta)) 
      case Array("max-stale", delta) => `max-stale`(HttpNumbers.parseHttpNumbers(delta))
      case Array("min-fresh", delta) => `min-fresh`(HttpNumbers.parseHttpNumbers(delta))
      case Array("no-transform") => `no-transform` 
      case Array("only-if-cached") => `only-if-cached` 
      case Array("public") => `public` 
      case Array("must-revalidate") => `must-revalidate` 
      case Array("proxy-revalidate") => `proxy-revalidate` 
      case Array("s-maxage", delta) => `s-maxage`(HttpNumbers.parseHttpNumbers(delta))
      case default => NullDirective("null")
    })
    return (noFieldDirectives ++ fieldDirectives).filterNot(x => x == NullDirective)
  }

  sealed abstract class RequestDirective extends CacheDirective 

  sealed abstract class ResponseDirective extends CacheDirective 

  /* Requests */

  case object `no-cache` extends RequestDirective 
  case object `no-store` extends RequestDirective 
  case class `max-age`(inDelta: Option[HttpNumber]) extends RequestDirective {
    override def delta = inDelta
  }
  case class `max-stale`(inDelta: Option[HttpNumber]) extends RequestDirective {
    override def delta = inDelta
  }
  case class `min-fresh`(inDelta: Option[HttpNumber]) extends RequestDirective {
    override def delta = inDelta
  }
  case object `no-transform` extends RequestDirective 
  case object `only-if-cached` extends RequestDirective
  case class CustomRequestDirective(inName: String)  extends RequestDirective {
    override def name = inName
  }

  /* Responses */

  case object `public` extends ResponseDirective   
  case class `private` (inFieldNames: Option[String]) extends ResponseDirective {
    override def fieldNames = inFieldNames
  }
  case class `no-cache`(inFieldNames: Option[String]) extends ResponseDirective {
    override def fieldNames = inFieldNames
  }
  //case object `no-store` extends ResponseDirective       //Probably should use some implicits here 
  //case object `no-transform` extends ResponseDirective  
  case object `must-revalidate` extends ResponseDirective  
  case object `proxy-revalidate` extends ResponseDirective
  //case class `max-age`(delta: Option[HttpNumber])  extends ResponseDirective  with NoFieldName 
  case class `s-maxage`(inDelta: Option[HttpNumber])  extends ResponseDirective { 
    override def delta = inDelta
  }

  case class CustomResponseDirective(inName: String) extends ResponseDirective {
    override def name = inName
  }

  case class NullDirective(inName: String) extends CacheDirective {
    override def name = inName
    override def value = ""
  }
}
