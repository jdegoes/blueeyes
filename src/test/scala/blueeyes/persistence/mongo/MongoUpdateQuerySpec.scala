package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._

class MongoUpdateQuerySpec extends Specification{


  "'where' method sets new filter" in {
    import MongoImplicits._

    val jObject = JObject(JField("Foo", JString("bar")) :: Nil)
    val query = update("collection").set(jObject)

    query.where("name" === "Joe") mustEqual ( MongoUpdateQuery("collection", jObject, Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}