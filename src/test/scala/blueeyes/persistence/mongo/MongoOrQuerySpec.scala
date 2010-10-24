package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoOrQuerySpec extends Specification{
  private val query1  = MongoQueryBuilder(JPath("foo")).>(MongoQueryImplicits.MongoPrimitiveInt(1))
  private val query2  = MongoQueryBuilder(JPath("bar")).<(MongoQueryImplicits.MongoPrimitiveInt(5))
  private val orQuery = query1 || query2
  
  "create valid json for or query" in {
    (orQuery).query mustEqual (JObject(JField("$or", JArray(query1.query :: query2.query :: Nil)) :: Nil))
  }
  "unary_! use 'and' use with negative operators of subquerys " in{
    (orQuery).unary_! mustEqual (query1.unary_! && query2.unary_!)
  }  
  "2 unary_! results to the same query" in{
    (orQuery).unary_!.unary_! mustEqual (orQuery)
  }
}