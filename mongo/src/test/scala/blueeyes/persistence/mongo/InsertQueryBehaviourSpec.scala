package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoQueryBuilder._
import blueeyes.json._
import org.specs2.mock.Mockito

class InsertQueryBehaviourSpec extends Specification with Mockito {
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  "Call collection method" in{
    val collection  = mock[DatabaseCollection]
    collection.getLastError returns None

    val query  = insert(jObject).into("collection")
    query(collection)

    there was one(collection).insert(jObject :: Nil)
  }
}
