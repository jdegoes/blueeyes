package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class EnsureIndexQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]
  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)

    val query  = ensureUniqueIndex("index").on("collection", "address.city", "address.street")
    val result = query(collection)
//    val countDown = new CountDownLatch(1)
//
//    result.deliverTo{v => countDown.countDown()}
//    countDown.await()

    Mockito.verify(collection, times(1)).ensureIndex("index", JPath("address.city") :: JPath("address.street") :: Nil, true)

    result must be (JNothing)
  }
}