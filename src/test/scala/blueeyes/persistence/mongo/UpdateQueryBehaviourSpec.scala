package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import MongoQueryBuilder._
import org.mockito.Matchers._
import blueeyes.json.JsonAST._
import org.specs2.mock.Mockito
import org.specs2.matcher.MustThrownMatchers

class UpdateQueryBehaviourSpec  extends Specification with Mockito with MustThrownMatchers{
  private val jObject = JObject(JField("address", JObject( JField("city", JString("London")) :: JField("street", JString("Regents Park Road")) ::  Nil)) :: Nil)

  "Call collection method" in{
    val collection  = mock[DatabaseCollection]
    collection.getLastError returns None

    val query  = update("collection").set(jObject).where("name" === "Joe")
    query(collection)

    there was one(collection).update(Some("name" === "Joe"), jObject, false, false)
  }

  "Does not call collection method when update is MongoUpdateNothing" in{
    val collection  = mock[DatabaseCollection]
    collection.getLastError returns None

    val query  = update("collection").set(MongoUpdateNothing).where("name" === "Joe")
    query(collection)

    there was no(collection).update(Some("name" === "Joe"), MongoUpdateNothing, false, false)
  }
}
