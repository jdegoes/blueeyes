package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class EnsureIndexQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]
  "Call collection method" in{
    import MongoImplicits._
    
    val query  = ensureUniqueIndex("index").on("collection", "address.city", "address.street")
    val result = query(collection)

    Mockito.verify(collection, times(1)).ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)

    result must be (JNothing)
  }
}