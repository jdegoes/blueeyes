package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoFilterBuilder._
import MongoFilterOperators._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class MongoSelectOneQuerySpec extends Specification{
  private val query = selectOne("foo", "bar").from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    import MongoImplicits._
    query.where("name" === "Joe") mustEqual (MongoSelectOneQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), "collection", Some(MongoFieldFilter("name", $eq, "Joe"))))
  }

  "'sortBy' method sets new sort" in {
    import MongoImplicits._
    query.sortBy("name" << ) mustEqual (MongoSelectOneQuery(MongoSelection(JPath("foo") :: JPath("bar") :: Nil), "collection", None, Some(MongoSort(JPath("name"), MongoSortOrderDescending))))
  }
}