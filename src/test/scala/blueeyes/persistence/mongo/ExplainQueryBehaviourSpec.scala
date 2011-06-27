package blueeyes.persistence.mongo

import org.specs.Specification
import org.specs.mock.MocksCreation
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.{JPath, JsonParser}

class ExplainQueryBehaviourSpec extends Specification with MocksCreation{
  private val collection  = mock[DatabaseCollection]
  private val explanation = JsonParser.parse("""{
    "cursor" : "BasicCursor",
    "nscanned" : 3,
    "nscannedObjects" : 3,
    "n" : 3,
    "millis" : 38,
    "nYields" : 0,
    "nChunkSkips" : 0,
    "isMultiKey" : false,
    "indexOnly" : false,
    "indexBounds" : {

    }
}""").asInstanceOf[JObject]

  private val keys     = MongoSelection(Set(JPath("foo"), JPath("bar")))

  "Call collection method" in{
    when(collection.getLastError).thenReturn(None)
    when(collection.explain(keys, None, None, None, None, None, false)).thenReturn(explanation)

    val query  = select("foo", "bar").from("collection").explain
    val result: JObject = query(collection)

    Mockito.verify(collection, times(1)).explain(keys, None, None, None, None, None, false)

    result mustEqual(explanation)
  }

}