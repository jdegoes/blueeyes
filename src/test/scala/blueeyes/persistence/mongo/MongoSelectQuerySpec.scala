package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterBuilder._
import MongoFilterOperators._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath


class MongoSelectQuerySpec extends Specification{
  private val query = select("foo", "bar").from("collection")

  "'where' method sets new filter" in {
    import MongoFilterImplicits._
    query.where("name" === "Joe") mustEqual (MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection"), Some(MongoFieldFilter("name", $eq, "Joe"))))
  }
  "'sortBy' method sets new sort" in {
    query.sortBy("name" << ) mustEqual (MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection"), None, Some(MongoSort(JPath("name"), MongoSortOrderDescending))))
  }
  "'skip' method sets new skip" in {
    query.skip(10) mustEqual (MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection"), None, None, Some(10)))
  }
  "'limit' method sets new limit" in {
    query.limit(10) mustEqual (MongoSelectQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), MongoCollection("collection"), None, None, None, Some(10)))
  }
}