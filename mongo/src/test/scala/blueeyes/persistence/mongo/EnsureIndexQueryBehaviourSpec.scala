package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoQueryBuilder._
import org.mockito.Matchers._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import org.specs2.mock.Mockito

class EnsureIndexQueryBehaviourSpec extends Specification with Mockito {
  "Call collection method" in {
    val collection  = mock[DatabaseCollection]
    collection.getLastError returns None

    val query = ensureUniqueIndex("index").on("address.city", "address.street").in("collection")
    query(collection)

    there was one(collection).ensureIndex("index", List(Tuple2(JPath("address.city"), OrdinaryIndex), Tuple2(JPath("address.street"), OrdinaryIndex)), true, JObject(Nil))
  }
}
