package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import com.mongodb.MongoException

class MongoQueryBehaviourSpec extends Specification {
  private val query       = new QueryBehaviours.MongoQueryBehaviour[Int]{
    def query(collection: DatabaseCollection): Int = 1
  }

  "MongoQueryBehaviourSpec: calls underlying query" in{
    val collection  = mock[DatabaseCollection]

    when(collection.getLastError).thenReturn(None)

    val result    = query(collection)

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