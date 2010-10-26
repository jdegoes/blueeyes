package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._

class EnsureIndexQueryBehaviourSpec extends Specification {
  "Call collection method" in{
    val collection  = mock[DatabaseCollection]

    val query = ensureUniqueIndex("index").on("collection", "address.city", "address.street")
    query(collection)    

    Mockito.verify(collection, times(1)).ensureIndex("index", jObject2MongoObject(JObject(JField("address.city", JInt(1)) :: JField("address.street", JInt(1)) :: Nil)), true)
  }
}