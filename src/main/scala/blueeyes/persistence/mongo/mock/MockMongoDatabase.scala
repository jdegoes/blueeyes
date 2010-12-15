package blueeyes.persistence.mongo.mock

import blueeyes.persistence.mongo._

private[mongo] class MockMongoDatabase() extends MongoDatabase{
  private val collections   = scala.collection.mutable.Map[String, MockDatabaseCollection]()

  def collection(collectionName: String) = {
    collections.get(collectionName) match{
      case Some(x) => x
      case None =>{
        val collection  = new MockDatabaseCollection()
        collections.put(collectionName, collection)
        collection
      }
    }
  }
}
