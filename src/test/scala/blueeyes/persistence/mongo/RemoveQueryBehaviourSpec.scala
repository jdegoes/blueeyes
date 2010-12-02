package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._

class RemoveQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]

  "Call collection method" in{
    import MongoImplicits._

    val filter = Some("name" === "Joe")

    val query  = remove.from("collection").where("name" === "Joe")
    val result = query(collection)

    Mockito.verify(collection, times(1)).remove(filter)

    result must be (JNothing)
  }
  "Call collection method with dummy JObject when filter is not specified" in{
    import MongoImplicits._

    val query = remove.from("collection")
    val result = query(collection)

    Mockito.verify(collection, times(1)).remove(None)
    
    result must be (JNothing)
  }
}