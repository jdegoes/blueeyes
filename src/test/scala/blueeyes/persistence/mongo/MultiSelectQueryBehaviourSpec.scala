package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import IterableViewImpl._
import org.specs2.matcher.MustThrownMatchers
import org.specs2.mock._

class MultiSelectQueryBehaviourSpec extends Specification with Mockito with MustThrownMatchers{
  private val keys      = MongoSelection(Set())

  private val jObject  = JObject(JField("address", JObject( JField("city", JString("A")) :: JField("street", JString("1")) ::  Nil)) :: Nil)
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)
  private val jObject2 = JObject(JField("address", JObject( JField("city", JString("C")) :: JField("street", JString("3")) ::  Nil)) :: Nil)

  "Collect all object if they are found" in{
    val collection  = mock[DatabaseCollection]
    val filter1     = ("address.city" === "A")
    val filter2     = ("address.street" === "2")
    val orFilter    = MongoOrFilter(List(filter1, filter2))
    
    collection.getLastError returns (None: Option[com.mongodb.BasicDBObject])
    collection.select(keys, Some(orFilter), None, None, None, None, false) returns new IterableViewImpl[JObject, Iterator[JObject]](List(jObject, jObject1, jObject2).iterator)

    val query  = multiSelect(filter1, filter2).from("collection")
    val result = query(collection)

    there was one(collection).select(keys, Some(orFilter), None, None, None, None, false)

    result.toList mustEqual (List(Some(jObject), Some(jObject1)))
  }
  "Collect all object if the same object matches more then one filter" in{
    val collection  = mock[DatabaseCollection]
    val filter1     = ("address.city" === "A")
    val filter2     = ("address.street" === "1")
    val filter3     = ("address.street" === "2")
    val orFilter    = MongoOrFilter(List(filter1, filter2, filter3))

    collection.getLastError returns None
    collection.select(keys, Some(orFilter), None, None, None, None, false) returns new IterableViewImpl[JObject, Iterator[JObject]](List(jObject, jObject1, jObject2).iterator)

    val query  = multiSelect(filter1, filter2, filter3).from("collection")
    val result = query(collection)

    there was one(collection).select(keys, Some(orFilter), None, None, None, None, false)

    result.toList mustEqual (List(Some(jObject), Some(jObject), Some(jObject1)))
  }
  "Miss some objects if they are not found" in{
    val collection  = mock[DatabaseCollection]
    val filter1     = ("address.city" === "A")
    val filter2     = ("address.street" === "7")
    val orFilter    = MongoOrFilter(List(filter1, filter2))

    collection.getLastError returns None
    collection.select(keys, Some(orFilter), None, None, None, None, false) returns new IterableViewImpl[JObject, Iterator[JObject]](List(jObject, jObject1, jObject2).iterator)

    val query  = multiSelect(filter1, filter2).from("collection")
    val result = query(collection)

    there was one(collection).select(keys, Some(orFilter), None, None, None, None, false)

    result.toList mustEqual (List(Some(jObject), None))
  }

}