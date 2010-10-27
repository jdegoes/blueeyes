package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._

class SelectQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  private val jObject  = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  "Call collection method" in{
    import MongoFilterImplicits._

    val keys   = jObject2MongoObject(JObject(JField("foo", JInt(1)) :: JField("bar", JInt(1)) :: Nil))
    val filter = jObject2MongoObject(JObject(JField("name", JString("Joe")) :: Nil))
    val sort   = Some(jObject2MongoObject(JObject(JField("name", JInt(-1)) :: Nil)))

    when(collection.select(keys, filter, sort, Some(2), Some(1))).thenReturn(jObject2MongoObject(jObject) :: jObject2MongoObject(jObject1) :: Nil)

    val query  = select("foo", "bar").from("collection").where("name" === "Joe").sortBy("name" <<).skip(2).limit(1)
    val result = query(collection)

    Mockito.verify(collection, times(1)).select(keys, filter, sort, Some(2), Some(1))

    result mustEqual(jObject :: jObject1 :: Nil)
  }
}