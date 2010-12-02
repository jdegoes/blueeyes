package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterBuilder._
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

class MongoMapReduceQuerySpec extends Specification{
  private val query = mapReduce("foo", "bar").from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    import MongoImplicits._
    query.where("name" === "Joe") mustEqual (MongoMapReduceQuery("foo", "bar",  "collection", None, Some(MongoFieldFilter("name", $eq, "Joe"))))
  }

  "'on' method set output collection name" in {
    import MongoImplicits._
    query.into("output") mustEqual (MongoMapReduceQuery("foo", "bar",  "collection", Some("output"), None))
  }
}