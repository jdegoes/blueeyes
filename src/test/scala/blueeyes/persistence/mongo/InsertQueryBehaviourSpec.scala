package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._

class InsertQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)
  "Call collection method" in{
    import MongoImplicits._
    
    val query  = insert(jObject).into("collection")
    val result = query(collection)

    Mockito.verify(collection, times(1)).insert(jObject2MongoObject(jObject) :: Nil)

    result must be (JNothing)
  }
}