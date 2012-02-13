package blueeyes.persistence.mongo

import net.lag.configgy.ConfigMap
import akka.util.Timeout
import blueeyes.core.service.test.MockConfiguration

/** ConfigurableMongo creates mongo database type based on JVM parameter "mongo.mock".
 * If the value is true then Mock Mongo is created otherwise Real Mongo is created.
 * <p>
 * It does not create a new Mock Mongo instance every time factor method is called,
 * It uses the same Mock Mongo instance.
 * <p>
 * If the configuration contains section "dropBeforeStart" then all specified collection(s) on specified database(s)
 * are dropped before starting.
 * <p>
 * Sample configuration is:
 * <p>
 * dropBeforeStart {
 *   mydb = ["mycollection"]
 * }
 *
 */
trait ConfigurableMongo extends MongoImplicits{
  private lazy val mockMongo = new MockMongo()

  def mongo(mongoConfig: ConfigMap): Mongo =  {
    val isMock = MockConfiguration.isMocked(ConfigurableMongo.MongoSwitch)
    val mongo  = if (isMock) mockMongo else RealMongo(mongoConfig)

    drop(mongo, mongoConfig.configMap("dropBeforeStart"))

    mongo
  }

  private def drop(mongo: Mongo, dropConfig: ConfigMap) {
    implicit val dropTimeout = Timeout(dropConfig.getLong("timeout").getOrElse(600000L))
    for (database <- dropConfig.keys.filterNot(_ == "timeout"); collection <- dropConfig.getList(database)) {
      mongo.database(database)(remove.from(collection))
    }
  }
}

object ConfigurableMongo{
  val MongoSwitch = "mongo.mock"
}
