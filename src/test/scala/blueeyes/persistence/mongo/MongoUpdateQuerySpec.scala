package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterOperators._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._

class MongoUpdateQuerySpec extends Specification{

  private val jObject = JObject(JField("Foo", JString("bar")) :: Nil)
  private val query = update("collection").set(jObject)

  "'where' method sets new filter" in {
    import MongoFilterImplicits._
    query.where("name" === "Joe") mustEqual ( MongoUpdateQuery(MongoCollection("collection"), jObject, Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}