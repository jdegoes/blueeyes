package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._

class RemoveQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)

    val filter = Some("name" === "Joe")

    val query  = remove.from("collection").where("name" === "Joe")
    val result = query(collection)
    val countDown = new CountDownLatch(1)

    result.deliverTo{v => countDown.countDown()}
    countDown.await()

    Mockito.verify(collection, times(1)).remove(filter)

    result.value must eventually (beSome(JNothing))
  }
  "Call collection method with dummy JObject when filter is not specified" in{
    when(collection.getLastError).thenReturn(None)

    val query = remove.from("collection")
    val result = query(collection)
    val countDown = new CountDownLatch(1)

    result.deliverTo{v => countDown.countDown()}
    countDown.await()        

    Mockito.verify(collection, times(1)).remove(None)
    
    result.value must eventually (beSome(JNothing))
  }
}