package blueeyes.persistence.mongo.mock

import com.google.inject.{AbstractModule, Provider, Inject}
import blueeyes.persistence.mongo.Mongo

class MockMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[MockMongoProvider])
  }
}

@com.google.inject.Singleton
private[mongo] class MockMongoProvider @Inject() () extends Provider[Mongo]{
  private val factory = new MockMongo()
  def get = factory
}

@com.google.inject.Singleton
private[mongo] class MockMongo() extends Mongo{
  private val databases     = scala.collection.mutable.Map[String, MockMongoDatabase]()
  def database(databaseName: String) = {
    databases.get(databaseName) match{
      case Some(x) => x
      case None =>{
        val mongoDatabase  = new MockMongoDatabase()
        databases.put(databaseName, mongoDatabase)
        mongoDatabase
      }
    }
  }
}
