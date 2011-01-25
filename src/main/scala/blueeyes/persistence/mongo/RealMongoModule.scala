package blueeyes.persistence.mongo

import net.lag.configgy.Config
import com.google.inject.{Provider, Inject, AbstractModule}

class RealMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[RealMongoProvider])
  }
}

@com.google.inject.Singleton
class RealMongoProvider @Inject() (config: Config) extends Provider[Mongo]{
  private val factory = new RealMongo(config.configMap("mongo"))
  def get = factory
}