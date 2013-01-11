package blueeyes.persistence.mongo

import org.specs2.mutable.Specification

import blueeyes.json.JPath
import blueeyes.json.JPathImplicits._
import blueeyes.json.JObject

import dsl._
import MongoFilterOperators._

class MongoGroupQuerySpec extends Specification{
  private val query = group(JObject(Nil), "dummy", "foo", "bar").from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    query.where("name" === "Joe") mustEqual (MongoGroupQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", JObject(Nil), "dummy", Some(MongoFieldFilter("name", $eq, "Joe"))))
  }
}
