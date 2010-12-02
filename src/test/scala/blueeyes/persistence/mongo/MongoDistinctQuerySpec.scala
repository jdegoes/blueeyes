package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterBuilder._
import MongoFilterOperators._
import blueeyes.json.JPath

class MongoDistinctQuerySpec extends Specification{
  "'where' method sets new filter" in {
    import MongoImplicits._

    val query = distinct("foo").from("collection")
    
    query.where("name" === "Joe") mustEqual (MongoDistinctQuery(JPath("foo"), "collection", Some(MongoFieldFilter("name", $eq, "Joe"))))
  }
}