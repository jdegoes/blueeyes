package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
import org.specs2.mock._

class MongoQueryBehaviourSpec extends Specification with Mockito{
  private object query extends QueryBehaviours.MongoQueryBehaviour {
    type QueryResult = Int
    def query(collection: DatabaseCollection): Int = 1
  }

  "MongoQueryBehaviourSpec: calls underlying query" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns None

    val result: Int    = query(collection)

    there was one(collection).requestStart
    there was one(collection).getLastError
    there was one(collection).requestDone


    result.value must eventually (be_==(1))
  }
  "MongoQueryBehaviourSpec: throw error when operation failed" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns Some(new com.mongodb.BasicDBObject())

    query(collection) must throwAn[MongoException]

    there was one(collection).requestStart
    there was one(collection).getLastError
    there was one(collection).requestDone
  }
}
