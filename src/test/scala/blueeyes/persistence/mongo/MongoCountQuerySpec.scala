package blueeyes.persistence.mongo

import org.specs.Specification
import MongoQueryBuilder._
import MongoFilterOperators._

class MongoCountQuerySpec extends Specification{
  private val query = count.from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    query.where("name" === "Joe") mustEqual ( MongoCountQuery("collection", Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}