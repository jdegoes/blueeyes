package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import MongoImplicits._
import blueeyes.json.JPathImplicits._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._

class RemoveQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  "Call collection method" in{
    import MongoFilterImplicits._

    val filter = Some("name" === "Joe")
    when(collection.remove(filter)).thenReturn(2)

    val query  = remove.from("collection").where("name" === "Joe")
    val result = query(collection)

    Mockito.verify(collection, times(1)).remove(filter)

    result mustEqual(JInt(2))
  }
  "Call collection method with dummy JObject when filter is not specified" in{
    import MongoFilterImplicits._

    when(collection.remove(None)).thenReturn(2)

    val query = remove.from("collection")
    val result = query(collection)

    Mockito.verify(collection, times(1)).remove(None)
    
    result mustEqual(JInt(2))
  }
}