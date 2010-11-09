package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.json.MongoJson._
import scala.collection.JavaConversions._
import java.lang.String
import blueeyes.json.JsonAST._
import com.mongodb._
import com.google.inject.{Provider, Inject}
import net.lag.configgy.Config
import blueeyes.json.{Printer, JPath}

private[mongo] object RealMongoImplementation{

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
    def apply[T](query: MongoQuery[T]): T  = query(collection(query.collection.name))
    def collection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName))
  }

  class RealDatabaseCollection(collection: DBCollection) extends DatabaseCollection{
    def insert(objects: List[JObject])      = checkWriteResult(collection.insert(objects.map(jObject2MongoObject(_))))

    def remove(filter: Option[MongoFilter]) = checkWriteResult(collection.remove(toMongoFilter(filter))).getN

    def count(filter: Option[MongoFilter])  = collection.getCount(toMongoFilter(filter))

    def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean) = checkWriteResult(collection.update(toMongoFilter(filter), value.toJValue, upsert, multi)).getN

    def ensureIndex(name: String, keys: List[JPath], unique: Boolean) = collection.ensureIndex(JObject(keys.map(key => JField(JPathExtension.toMongoField(key), JInt(1)))), name, unique)

    def dropIndex(name: String) = collection.dropIndex(name)

    def dropIndexes = collection.dropIndexes()

    def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {

      val keysObject   = toMongoKeys(selection)
      val filterObject = toMongoFilter(filter)
      val sortObject   = sort.map(v => JObject(JField(JPathExtension.toMongoField(v.sortField), JInt(v.sortOrder.order)) :: Nil)).map(jObject2MongoObject(_))

      val cursor        = collection.find(filterObject, keysObject)
      val sortedCursor  = sortObject.map(cursor.sort(_)).getOrElse(cursor)
      val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
      val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)

      stream(limitedCursor.iterator)
    }

    def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray = {
      val result = collection.group(toMongoKeys(selection), toMongoFilter(filter), initial, reduce)

      JArray(mongoObject2JObject(result.asInstanceOf[DBObject]).fields.map(_.value))
    }

    def distinct(selection: JPath, filter: Option[MongoFilter]) = {
      val key    = JPathExtension.toMongoField(selection)
      val result = filter.map(v => collection.distinct(key, v.filter.asInstanceOf[JObject])).getOrElse(collection.distinct(key))
      
      mongoObject2JObject(result.asInstanceOf[DBObject]).fields.map(_.value)
    }

    def stream(dbObjectsIterator: java.util.Iterator[com.mongodb.DBObject]): Stream[JObject] =
      if (dbObjectsIterator.hasNext) Stream.cons(mongoObject2JObject(dbObjectsIterator.next), stream(dbObjectsIterator)) else Stream.empty

    private def toMongoKeys(selection : MongoSelection)    = JObject(selection.selection.map(key => JField(JPathExtension.toMongoField(key), JInt(1))))
    private def toMongoFilter(filter: Option[MongoFilter]) = jObject2MongoObject(filter.map(_.filter.asInstanceOf[JObject]).getOrElse(JObject(Nil)))

    private def checkWriteResult(result: WriteResult) = {
      val error  = result.getLastError
      if (error != null && error.get("err") != null){
       throw new MongoException(error)
      }
      result
    }
  }

}
