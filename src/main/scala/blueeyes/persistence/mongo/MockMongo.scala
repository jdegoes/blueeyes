package blueeyes.persistence.mongo

import blueeyes.json.JsonAST.JObject
import blueeyes.json.JPath


object MockMongo{
  import com.google.inject.{Provider, Inject, AbstractModule}

  class MockMongoModule extends AbstractModule {
    override def configure(): Unit = {
      bind(classOf[Mongo]).toProvider(classOf[MockMongoProvider])
    }
  }

  @com.google.inject.Singleton
  class MockMongoProvider @Inject() () extends Provider[Mongo]{
    private val factory = new MockMongo()
    def get = factory
  }

  @com.google.inject.Singleton
  class MockMongo() extends Mongo{
    def database(databaseName: String) = new MockMongoDatabase()
  }

  class MockMongoDatabase() extends MongoDatabase{
    def apply[T](query: MongoQuery[T]): T     = query(getCollection(query.collection.name))
    def getCollection(collectionName: String) = new MockDatabaseCollection()
  }

  class MockDatabaseCollection() extends DatabaseCollection{
    def insert(objects: List[JObject])      = {}

    def remove(filter: Option[MongoFilter]) = {0}

    def update(filter: Option[MongoFilter], value : JObject, upsert: Boolean, multi: Boolean) = {0}

    def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = {}

    def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
      Nil
    }
  }
}
