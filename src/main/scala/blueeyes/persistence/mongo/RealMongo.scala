package blueeyes.persistence.mongo

import collection.immutable.List
import blueeyes.persistence.mongo.json.MongoJson._
import scala.collection.JavaConversions._
import blueeyes.json.JPath
import java.lang.String
import blueeyes.json.JsonAST.{JInt, JField, JObject}
import com.mongodb._
import com.google.inject.{Provider, Inject, AbstractModule}
import net.lag.configgy.Config

object MongoConfiguration {
  val MongoHost = "mongo.host"
  val MongoPort = "mongo.port"
}

class RealMongoModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Mongo]).toProvider(classOf[RealMongoImplelemntation.RealMongoProvider])
  }
}

private[mongo] object RealMongoImplelemntation{

  @com.google.inject.Singleton
  class RealMongoProvider @Inject() (config: Config) extends Provider[Mongo]{
    private val factory = new RealMongo(config)
    def get = factory
  }

  @com.google.inject.Singleton
  class RealMongo(config: Config) extends Mongo{
    import MongoConfiguration._

    private lazy val mongo = new com.mongodb.Mongo(config.getString(MongoHost).getOrElse("MongoHost is not configured. Configure the value '%s'".format(MongoHost)) , config.getInt(MongoPort, ServerAddress.defaultPort()))

    def database(databaseName: String) = new RealMongoDatabase(mongo.getDB(databaseName))
  }

  class RealMongoDatabase(database: DB) extends MongoDatabase{
    def apply[T](query: MongoQuery[T]): T     = query(collection(query.collection.name))
    def collection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName))
  }

  class RealDatabaseCollection(collection: DBCollection) extends DatabaseCollection{
    def insert(objects: List[JObject])      = checkWriteResult(collection.insert(objects.map(jObject2MongoObject(_))))

    def remove(filter: Option[MongoFilter]) = checkWriteResult(collection.remove(jObject2MongoObject(filter.map(_.filter).getOrElse(JObject(Nil))))).getN

    def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean) = checkWriteResult(collection.update(jObject2MongoObject(filter.map(_.filter).getOrElse(JObject(Nil))), value.toJValue, upsert, multi)).getN

    def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = collection.ensureIndex(JObject(keys.map(key => JField(JPathExtension.toMongoField(key), JInt(1)))), name, unique)

    def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {

      val keysObject   = JObject(selection.selection.map(key => JField(JPathExtension.toMongoField(key), JInt(1))))
      val filterObject = filter.map(_.filter).getOrElse(JObject(Nil))
      val sortObject   = sort.map(v => JObject(JField(JPathExtension.toMongoField(v.sortField), JInt(v.sortOrder.order)) :: Nil)).map(jObject2MongoObject(_))

      val cursor        = collection.find(filterObject, keysObject)
      val sortedCursor  = sortObject.map(cursor.sort(_)).getOrElse(cursor)
      val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
      val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)

      List(limitedCursor.toArray: _*).map(mongoObject2JObject(_))
    }

    private def checkWriteResult(result: WriteResult) = {
      val error  = result.getLastError
      if (error != null && error.get("err") != null){
       throw new MongoException(error)
      }
      result
    }
  }

}
