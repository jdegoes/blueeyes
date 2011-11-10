package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoFilterOperators._
import blueeyes.json.JPath
import blueeyes.json.JPathImplicits._

class MongoEnsureIndexQuerySpec extends Specification with MongoImplicits{
  private val query = ensureIndex("foo").on("bar").in("collection")

  "'geospatial' method sets geospatial index type" in {
    query.geospatial("bar") mustEqual (MongoEnsureIndexQuery("collection", "foo", List(Tuple2(JPath("bar"), GeospatialIndex)), false))
  }
}