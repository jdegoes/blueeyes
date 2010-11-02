package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class SelectQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  private val keys     = MongoSelection(JPath("foo") :: JPath("bar") :: Nil)
  private val filter   = Some(MongoFilterBuilder("name") === MongoFilterImplicits.MongoPrimitiveString("Joe"))
  private val sort     = Some(MongoSort("name", MongoSortOrderDescending))
  private val jObject  = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  "Call collection method" in{
    import MongoFilterImplicits._

    when(collection.select(keys, filter, sort, Some(2), Some(1))).thenReturn(jObject :: jObject1 :: Nil)

    val query  = select("foo", "bar").from("collection").where("name" === "Joe").sortBy("name" <<).skip(2).limit(1)
    val result = query(collection)

    Mockito.verify(collection, times(1)).select(keys, filter, sort, Some(2), Some(1))

    result mustEqual(jObject :: jObject1 :: Nil)
  }

  "Call collection method with dummy JObject when filter is not specified" in{
    import MongoFilterImplicits._

    when(collection.select(keys, None, None, None, None)).thenReturn(jObject :: jObject1 :: Nil)

    val query  = select("foo", "bar").from("collection")
    val result = query(collection)

    Mockito.verify(collection, times(1)).select(keys, None, None, None, None)

    result mustEqual(jObject :: jObject1 :: Nil)
  }

}