package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoFilterOperators._
import dsl._

import blueeyes.json.JPathImplicits._
import blueeyes.json._

class MongoUpdateQuerySpec extends Specification{
  "'where' method sets new filter" in {
    val jObject = JObject(JField("Foo", JString("bar")) :: Nil)
    val query = update("collection").set(jObject)

    query.where("name" === "Joe") mustEqual ( MongoUpdateQuery("collection", jObject, Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}
