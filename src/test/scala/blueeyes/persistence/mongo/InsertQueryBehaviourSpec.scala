package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.Mockito
import org.specs.util.TimeConversions._
import MongoQueryBuilder._
import org.mockito.Matchers._
import blueeyes.persistence.mongo.json.MongoJsonBijection._
import blueeyes.json.JsonAST._

class InsertQueryBehaviourSpec extends Specification with Mockito {
  private val collection  = mock[DatabaseCollection]
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  "Call collection method" in{

    collection.getLastError returns None

    val query  = insert(jObject).into("collection")
    query(collection)

    there was one(collection).insert(unapply(jObject) :: Nil)
  }
}
