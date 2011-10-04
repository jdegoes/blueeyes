package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.Mockito
import MongoQueryBuilder._
import org.mockito.Matchers._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class EnsureIndexQueryBehaviourSpec extends Specification with Mockito {
  private val collection  = mock[DatabaseCollection]
  "Call collection method" in {
    collection.getLastError returns None

    val query = ensureUniqueIndex("index").on("address.city", "address.street").in("collection")
    query(collection)

    there was one(collection).ensureIndex("index", List(Tuple2(JPath("address.city"), OrdinaryIndex), Tuple2(JPath("address.street"), OrdinaryIndex)), true, JObject(Nil))
  }
}
