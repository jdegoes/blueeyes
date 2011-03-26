package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._

class UpdateQueryBehaviourSpec  extends Specification {
  private val collection  = mock[DatabaseCollection]
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)

    val filter   = Some("name" === "Joe")

    val query  = update("collection").set(jObject).where("name" === "Joe")
    val result = query(collection)
//    val countDown = new CountDownLatch(1)
//
//    result.deliverTo{v => countDown.countDown()}
//    countDown.await()

    Mockito.verify(collection, times(1)).update(filter, jObject, false, false)

    result must be (JNothing)
  }
  "Does not call collection method when update is MongoUpdateNothing" in{
    when(collection.getLastError).thenReturn(None)

    val filter   = Some("name" === "Joe")

    val query  = update("collection").set(MongoUpdateNothing).where("name" === "Joe")
    val result = query(collection)
//    val countDown = new CountDownLatch(1)
//
//    result.deliverTo{v => countDown.countDown()}
//    countDown.await()

    Mockito.verify(collection, times(0)).update(filter, MongoUpdateNothing, false, false)

    result must be (JNothing)
  }
}