package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.MocksCreation
import org.specs.util.TimeConversions._
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._

class InsertQueryBehaviourSpec extends Specification with MocksCreation{
  private val collection  = mock[DatabaseCollection]
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  "Call collection method" in{

    when(collection.getLastError).thenReturn(None)

    val query  = insert(jObject).into("collection")
    val result: JValue = query(collection)

    Mockito.verify(collection, times(1)).insert(jObject2MongoObject(jObject) :: Nil)

    result must be (JNothing)
  }
}