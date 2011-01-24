package blueeyes.persistence.mongo.mock

import com.google.inject.{AbstractModule, Provider, Inject}
import blueeyes.persistence.mongo.Mongo

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
