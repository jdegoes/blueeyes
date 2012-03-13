package blueeyes.persistence.mongo

import org.streum.configrity.Configuration 
import akka.util.Timeout

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

  def mongo(mongoConfig: Configuration): Mongo =  {
    val isMock = sys.props.getOrElse(ConfigurableMongo.MongoSwitch, "false").toBoolean
    val mongo  = if (isMock) mockMongo else RealMongo(mongoConfig)

    drop(mongo, mongoConfig.detach("dropBeforeStart"))

    mongo
  }

  private def drop(mongo: Mongo, dropConfig: Configuration) {
    implicit val dropTimeout = Timeout(dropConfig.get[Long]("timeout").getOrElse(600000L))
    for (database <- dropConfig.data.keys.filterNot(_ == "timeout"); collection <- dropConfig[List[String]](database, List())) {
      mongo.database(database)(remove.from(collection))
    }
  }
}

object ConfigurableMongo{
  val MongoSwitch = "mongo.mock"
}
