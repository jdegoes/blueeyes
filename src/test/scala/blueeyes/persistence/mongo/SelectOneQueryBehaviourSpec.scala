package blueeyes.persistence.mongo

import java.util.concurrent.CountDownLatch
import org.spex.Specification
import MongoQueryBuilder._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class SelectOneQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  private val keys     = MongoSelection(JPath("foo") :: JPath("bar") :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)
    when(collection.select(keys, None, None, None, Some(1))).thenReturn(new IterableViewImpl[JObject](List(jObject1).iterator))

    val query  = selectOne("foo", "bar").from("collection")
    val result = query(collection)
//    val countDown = new CountDownLatch(1)
//
//    result.deliverTo{v => countDown.countDown()}
//    countDown.await()

    Mockito.verify(collection, times(1)).select(keys, None, None, None, Some(1))

    result must (beSome(jObject1))
  }

}