package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.MocksCreation
import MongoQueryBuilder._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class SelectOneQueryBehaviourSpec extends Specification with MocksCreation{
  private val collection  = mock[DatabaseCollection]

  private val keys     = MongoSelection(Set(JPath("foo"), JPath("bar")))
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)
    when(collection.select(keys, None, None, None, Some(1), None, false)).thenReturn(new IterableViewImpl[JObject, Iterator[JObject]](List(jObject1).iterator))

    val query  = selectOne("foo", "bar").from("collection")
    val result: Option[JObject] = query(collection)

    Mockito.verify(collection, times(1)).select(keys, None, None, None, Some(1), None, false)

    result must (beSome(jObject1))
  }

}