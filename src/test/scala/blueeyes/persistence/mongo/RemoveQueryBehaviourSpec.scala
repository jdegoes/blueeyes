package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.Mockito
import MongoQueryBuilder._
import org.mockito.Matchers._
import blueeyes.json.JsonAST._

class RemoveQueryBehaviourSpec extends Specification with Mockito {
  private val collection  = mock[DatabaseCollection]

  "Call collection method" in{
    collection.getLastError returns None

    val filter = Some("name" === "Joe")

    val query  = remove.from("collection").where("name" === "Joe")
    query(collection)

    there was one(collection).remove(filter)
  }
  "Call collection method with dummy JObject when filter is not specified" in{
    collection.getLastError returns None

    val query = remove.from("collection")
    query(collection)

    there was one(collection).remove(None)
  }
}
