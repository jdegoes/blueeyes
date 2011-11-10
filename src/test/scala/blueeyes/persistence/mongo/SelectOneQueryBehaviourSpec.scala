package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoQueryBuilder._
import blueeyes.json.JPathImplicits._
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import org.specs2.mock._
import org.specs2.matcher.MustThrownMatchers

class SelectOneQueryBehaviourSpec extends Specification with Mockito with MustThrownMatchers{

  private val keys     = MongoSelection(Set(JPath("foo"), JPath("bar")))
  private val jObject1 = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road 1")) ::  Nil)) :: Nil)

  "Call collection method" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns None
    collection.select(keys, None, None, None, Some(1), None, false) returns new IterableViewImpl[JObject, Iterator[JObject]](List(jObject1).iterator)

    val query  = selectOne("foo", "bar").from("collection")
    val result: Option[JObject] = query(collection)

    there was one(collection).select(keys, None, None, None, Some(1), None, false)

    result must (beSome(jObject1))
  }

}