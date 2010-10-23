package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.Specification
import MongoQueryOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json._
import blueeyes.persistence.mongo.json._

class MongoOrQuerySpec extends Specification{
  "create valid json for or query" in {
    import MongoQueryImplicits._
    val query1 = MongoQueryBuilder(JPath("foo")).>(1)
    val query2 = MongoQueryBuilder(JPath("bar")).<(5)

    (query1 || query2).query mustEqual (JObject(JField("$or", JArray(query1.query :: query2.query :: Nil)) :: Nil))
  }
}