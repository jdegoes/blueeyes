package blueeyes.core.http

import scala.util.matching.Regex
import blueeyes.util.ProductPrefixUnmangler

sealed trait CacheDirective extends ProductPrefixUnmangler {

  def name: String = unmangledName
  def delta: Option[HttpNumber] = None
  def fieldNames: Option[String] = None
  def value = (List(name) ++ delta.toList.map(_.value) ++ fieldNames.toList).mkString("=")
  override def toString = value;
}

object CacheDirectives {

  def parseCacheDirectives(inString: String): Array[CacheDirective] ={
    def noFieldRegex = new Regex("""([a-z\-]+)((=(\d)+)|(="[a-zA-Z-]+"))?""" )

    def directives: Array[CacheDirective] = (noFieldRegex.findAllIn(inString)).toArray.map(_.trim.split("=")).map( _ match {
      case Array("private", any)      => Some(`private`(Some(any)))
      case Array("no-cache", any)     => Some(`no-cache`(Some(any)))
      case Array("private")           => Some(`private`(None))
      case Array("no-cache")          => Some(`no-cache`)
      case Array("no-store")          => Some(`no-store`)
      case Array("max-age", delta)    => Some(`max-age`(HttpNumbers.parseHttpNumbers(delta)))
      case Array("max-stale", delta)  => Some(`max-stale`(HttpNumbers.parseHttpNumbers(delta)))
      case Array("min-fresh", delta)  => Some(`min-fresh`(HttpNumbers.parseHttpNumbers(delta)))
      case Array("no-transform")      => Some(`no-transform`)
      case Array("only-if-cached")    => Some(`only-if-cached`)
      case Array("public")            => Some(`public`)
      case Array("must-revalidate")   => Some(`must-revalidate`)
      case Array("proxy-revalidate")  => Some(`proxy-revalidate`)
      case Array("s-maxage", delta)   => Some(`s-maxage`(HttpNumbers.parseHttpNumbers(delta)))
      case default                    => None
    }).filterNot(_ == None).map(_.get)
    return directives
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
