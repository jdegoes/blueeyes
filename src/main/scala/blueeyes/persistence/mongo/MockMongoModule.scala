package blueeyes.persistence.mongo

import com.google.inject.{AbstractModule}

class MockMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[MockMongoImplementation.MockMongoProvider])
  }
}