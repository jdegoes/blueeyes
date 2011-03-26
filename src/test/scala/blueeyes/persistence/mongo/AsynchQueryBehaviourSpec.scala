package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

/*class AsynchQueryBehaviourSpec extends Specification {
  private val query       = new QueryBehaviours.AsynchQueryBehaviour[Int]{
    def query(collection: DatabaseCollection): Int = 1
  }

  "AsynchQueryBehaviour: calls underlying query" in{
    val collection  = mock[DatabaseCollection]

    when(collection.getLastError).thenReturn(None)

    val result    = query(collection)
    val countDown = new CountDownLatch(1)

    result.deliverTo{v => countDown.countDown()}
    countDown.await()

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone


    result.value must eventually (beSome (1))
  }
  "AsynchQueryBehaviour: cancel future when operation failed" in{
    import org.specs.util.TimeConversions._
    val collection  = mock[DatabaseCollection]

    when(collection.getLastError).thenReturn(Some(new com.mongodb.BasicDBObject()))

    val result    = query(collection)
    val countDown = new CountDownLatch(1)

    result.ifCanceled{v => countDown.countDown()}    
    countDown.await()

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone

    result.isCanceled must eventually (be (true))
  }
}*/