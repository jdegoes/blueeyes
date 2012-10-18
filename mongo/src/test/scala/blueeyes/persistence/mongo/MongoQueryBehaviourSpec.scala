package blueeyes.persistence.mongo

import org.specs2.mutable.Specification
import blueeyes.json._
import com.mongodb.MongoException
import org.specs2.mock._

class MongoQueryBehaviourSpec extends Specification with Mockito{
  private object verifiableQuery extends QueryBehaviours.MongoQueryBehaviour {
    val isVerifiable = true
    type QueryResult = Int
    def query(collection: DatabaseCollection): Int = 1
  }

  private object unverifiableQuery extends QueryBehaviours.MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Int
    def query(collection: DatabaseCollection): Int = 1
  }

  "MongoQueryBehaviourSpec: calls underlying verifiable query" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns None

    val result: Int    = verifiableQuery(collection)

    there was one(collection).requestStart
    there was one(collection).getLastError
    there was one(collection).requestDone


    result.value must eventually (be_==(1))
  }

  "MongoQueryBehaviourSpec: calls underlying unverifiable query" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns None

    val result: Int    = unverifiableQuery(collection)

    there was no(collection).requestStart
    there was no(collection).getLastError
    there was no(collection).requestDone

    result.value must eventually (be_==(1))
  }

  "MongoQueryBehaviourSpec: throw error when verifiable operation failed" in{
    val collection  = mock[DatabaseCollection]

    collection.getLastError returns Some(new com.mongodb.BasicDBObject())

    verifiableQuery(collection) must throwAn[MongoException]

    there was one(collection).requestStart
    there was one(collection).getLastError
    there was one(collection).requestDone
  }
}
