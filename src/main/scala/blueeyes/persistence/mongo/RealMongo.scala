package blueeyes.persistence.mongo

import com.google.inject.{AbstractModule}

object MongoConfiguration {
  val MongoHost = "mongo.host"
  val MongoPort = "mongo.port"
}

class RealMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[RealMongoImplementation.RealMongoProvider])
  }
}