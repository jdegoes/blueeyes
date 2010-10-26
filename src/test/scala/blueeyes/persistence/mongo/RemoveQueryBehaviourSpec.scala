package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times}
import org.mockito.Mockito
import blueeyes.persistence.mongo.json.MongoJson._
import blueeyes.json.JsonAST._

class RemoveQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  "Call collection method" in{
    import MongoFilterImplicits._
    val query = remove.from("collection").where("name" === "Joe")
    query(collection)

    Mockito.verify(collection, times(1)).remove(jObject2MongoObject(JObject(JField("name", JString("Joe")) :: Nil)))
  }
  "Call collection method with dummy JObject when filter is not specified" in{
    import MongoFilterImplicits._
    val query = remove.from("collection")
    query(collection)

    Mockito.verify(collection, times(1)).remove(jObject2MongoObject(JObject(Nil)))
  }
}