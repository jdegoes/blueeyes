package blueeyes.persistence.mongo

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
    import MongoImplicits._
    
    when(collection.select(keys, None, None, None, Some(1))).thenReturn(List(jObject1).toStream)

    val query  = selectOne("foo", "bar").from("collection")
    val result = query(collection)

    Mockito.verify(collection, times(1)).select(keys, None, None, None, Some(1))

    result mustEqual(Some(jObject1))
  }

}