package blueeyes.persistence.mongo

import org.spex.Specification
import MongoQueryBuilder._
import org.mockito.Mockito.{times, when}
import org.mockito.Mockito
import blueeyes.json.JsonAST._
import blueeyes.json.JPath

class VerifiedQueryBehaviourSpec extends Specification {
  private val collection  = mock[DatabaseCollection]
  private val query       = mock[MongoQuery[Int]]

  "VerifiedQueryBehaviour: calls underlying query" in{

    when(collection.getLastError).thenReturn(None)    
    when(query.apply(collection)).thenReturn(1)

    val verifiedQuery  = verified[Int](query)
    val result         = verifiedQuery(collection)

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(query, times(1)).apply(collection)
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone

    result must be (1)
  }
  "VerifiedQueryBehaviour: throw error when operation failed" in{

    when(collection.getLastError).thenReturn(Some(new com.mongodb.BasicDBObject()))
    when(query.apply(collection)).thenReturn(1)

    val verifiedQuery  = verified[Int](query)
    verifiedQuery(collection) must throwAnException[com.mongodb.MongoException]

    Mockito.verify(collection, times(1)).requestStart
    Mockito.verify(query, times(1)).apply(collection)
    Mockito.verify(collection, times(1)).getLastError
    Mockito.verify(collection, times(1)).requestDone
  }
}