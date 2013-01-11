package blueeyes.persistence.mongo

import org.specs2.mutable.Specification

import dsl._
import MongoFilterOperators._

class MongoRemoveQuerySpec extends Specification{

  "'where' method sets new filter" in {
    val query = remove.from("collection")
    query.where("name" === "Joe") mustEqual ( MongoRemoveQuery("collection", Some(MongoFieldFilter("name", $eq, "Joe"))) )
  }
}
