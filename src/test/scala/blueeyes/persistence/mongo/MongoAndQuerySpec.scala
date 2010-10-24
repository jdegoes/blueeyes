package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoAndQuerySpec extends Specification{
  private val query1    = MongoQueryBuilder(JPath("foo")).>(MongoQueryImplicits.MongoPrimitiveInt(1))
  private val query2    = MongoQueryBuilder(JPath("bar")).<(MongoQueryImplicits.MongoPrimitiveInt(5))
  private val andQuery  = query1 && query2

  "create valid json for or query" in {
    (andQuery).query mustEqual (JObject(query1.query.fields ++ query2.query.fields))
  }
  "unary_! use 'or' use with negative operators of subquerys " in{
    (andQuery).unary_! mustEqual (query1.unary_! || query2.unary_!)
  }

  "2 unary_! result to the same query" in{
    (andQuery).unary_!.unary_! mustEqual (andQuery)
  }
}