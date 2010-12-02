package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterOperators._

class MongoCountQuerySpec extends Specification{
  private val query = count.from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    import MongoImplicits._
    query.where("name" === "Joe") mustEqual ( MongoCountQuery("collection", Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}