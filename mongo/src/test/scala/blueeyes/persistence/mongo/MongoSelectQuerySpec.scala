package blueeyes.persistence.mongo

import org.specs2.mutable.Specification

import dsl._
import MongoFilterOperators._
import blueeyes.json._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JPath

class MongoSelectQuerySpec extends Specification {
  private val query = select("foo", "bar").from(MongoCollectionReference("collection"))

  "'where' method sets new filter" in {
    query.where(JPath("name") === "Joe") mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", Some(MongoFieldFilter("name", $eq, "Joe"))))
  }
  "'sortBy' method sets new sort" in {
    query.sortBy(JPath("name") <<) mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", sort = Some(MongoSort(JPath("name"), MongoSortOrderDescending))))
  }
  "'skip' method sets new skip" in {
    query.skip(10) mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, Some(10)))
  }
  "'snapshot' method sets snapshot mode" in {
    query.snapshot mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, None, None, None, true))
  }
  "'limit' method sets new limit" in {
    query.limit(10) mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, None, Some(10)))
  }
  "'hint' with name sets new hint" in {
    query.hint("foo") mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, None, None, Some(NamedHint("foo"))))
  }
  "'hint' with keys sets new hint" in {
    query.hint(JPath("foo") :: JPath("bar") :: Nil) mustEqual (MongoSelectQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, None, None, Some(KeyedHint(List(JPath("foo"), JPath("bar"))))))
  }
  "'explain' creates Explain query" in {
    query.hint(JPath("foo") :: JPath("bar") :: Nil).explain mustEqual (MongoExplainQuery(MongoSelection(Set(JPath("foo"), JPath("bar"))), "collection", None, None, None, None, Some(KeyedHint(List(JPath("foo"), JPath("bar"))))))
  }
}
