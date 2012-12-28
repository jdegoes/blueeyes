package blueeyes.persistence.mongo

import dsl._
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

import org.specs2.mutable.Specification

class MongoMapReduceQuerySpec extends Specification{
  private val query = mapReduce("foo", "bar").from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    query.where("name" === "Joe") mustEqual (MongoMapReduceQuery("foo", "bar",  "collection", None, Some(MongoFieldFilter("name", $eq, "Joe"))))
  }

  "'on' method set output collection name" in {
    query.into("output") mustEqual (MongoMapReduceQuery("foo", "bar",  "collection", Some("output"), None))
  }
}
