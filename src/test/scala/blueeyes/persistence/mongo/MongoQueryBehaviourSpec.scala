package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.MocksCreation
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import com.mongodb.MongoException

class MongoQueryBehaviourSpec extends Specification with MocksCreation{
  private val query       = new QueryBehaviours.MongoQueryBehaviour[Int]{
    def query(collection: DatabaseCollection): Int = 1
  }

  "MongoQueryBehaviourSpec: calls underlying query" in{
    val collection  = mock[DatabaseCollection]

    when(collection.getLastError).thenReturn(None)

    val result: Int    = query(collection)

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone


    result.value must eventually (be(1))
  }
  "MongoQueryBehaviourSpec: throw error when operation failed" in{
    import org.specs.util.TimeConversions._
    val collection  = mock[DatabaseCollection]

    when(collection.getLastError).thenReturn(Some(new com.mongodb.BasicDBObject()))

    query(collection) must throwAnException[MongoException]

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone
  }
}