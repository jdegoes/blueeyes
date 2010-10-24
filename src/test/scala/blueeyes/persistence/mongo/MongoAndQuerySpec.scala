package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JsonAST._
import blueeyes.json._

class MongoAndQuerySpec extends Specification{
  "create valid json for or query" in {
    import MongoQueryImplicits._
    
    val query1 = MongoQueryBuilder(JPath("foo")).>(1)
    val query2 = MongoQueryBuilder(JPath("bar")).<(5)

    (query1 && query2).query mustEqual (JObject(query1.query.fields ++ query2.query.fields))
  }
  "unary_! use 'or' use with negative operators of subquerys " in{
    import MongoQueryImplicits._
    val query1 = MongoQueryBuilder(JPath("foo")).>(1)
    val query2 = MongoQueryBuilder(JPath("bar")).<(5)

    (query1 && query2).unary_! mustEqual (query1.unary_! || query2.unary_!)
  }

}