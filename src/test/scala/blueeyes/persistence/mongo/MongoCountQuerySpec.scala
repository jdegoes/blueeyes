package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterOperators._
import MongoImplicits._
import blueeyes.json.JPathImplicits._

class MongoCountQuerySpec extends Specification{
  private val query = count.from("collection")

  "'where' method sets new filter" in {
    import MongoFilterImplicits._
    query.where("name" === "Joe") mustEqual ( MongoCountQuery("collection", Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}