package blueeyes.util

import QueryParser._
import org.specs.Specification
import org.specs.util._
import java.net.URI

class QueryParserSpec extends Specification {
  val baseURI = "http://www.socialmedia.com/test?"
    
  "Support 'normal' query params" in {
    val queryParams = "a=1&b=2"
    val query = URI.create(baseURI + queryParams).getRawQuery()
    unparseQuery(parseQuery(query)) must beEqual(queryParams)
  }
}