package blueeyes.persistence.mongo

import dsl._
import org.specs2.mutable.Specification
import blueeyes.json._
import blueeyes.json.{JPath, JParser}
import org.specs2.mock._

import scalaz.{Success, Validation}

class ExplainQueryBehaviourSpec extends Specification with Mockito{
  private val explanation: JObject = JParser.parseFromString("""{
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
}""").map(_.asInstanceOf[JObject]).valueOr { e => throw e }

  private val keys     = MongoSelection(Set(JPath("foo"), JPath("bar")))

  "Call collection method" in{
    val collection  = mock[DatabaseCollection]
    collection.getLastError returns None
    collection.explain(keys, None, None, None, None, None, false) returns explanation

    val query  = select("foo", "bar").from("collection").explain
    val result: JObject = query(collection)

    there was one(collection).explain(keys, None, None, None, None, None, false)
    result mustEqual(explanation)
  }

}
