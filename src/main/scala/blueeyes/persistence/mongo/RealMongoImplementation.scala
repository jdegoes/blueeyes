package blueeyes.persistence.mongo

import blueeyes.persistence.mongo.json.MongoJson._
import scala.collection.JavaConversions._
import blueeyes.json.JsonAST._
import com.mongodb._
import net.lag.configgy.ConfigMap
import blueeyes.json.{JPath}

@com.google.inject.Singleton
class RealMongo(config: ConfigMap) extends Mongo{
  val ServerAndPortPattern = "(.+):(.+)".r

  private lazy val mongo = {
    val servers = config.getList("servers").map(server =>{
      server match{
        case ServerAndPortPattern(host, port) => new ServerAddress(host.trim(), port.trim().toInt)
        case _ => new ServerAddress(server, ServerAddress.defaultPort())
      }
    }).toList
    val mongo = servers match {
      case x :: Nil => new com.mongodb.Mongo(x)
      case x :: xs  => new com.mongodb.Mongo(servers)
      case Nil => error("""MongoServers are not configured. Configure the value 'servers'. Format is '["host1:port1", "host2:port2", ...]'""")
    }
    if (config.getBool("slaveOk", true)) { mongo.slaveOk() }
    mongo
  }

  def database(databaseName: String) = new RealMongoDatabase(mongo.getDB(databaseName))
}

private[mongo] class RealMongoDatabase(database: DB) extends MongoDatabase{
  protected def collection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName))
}

private[mongo] class RealDatabaseCollection(collection: DBCollection) extends DatabaseCollection{

  def requestDone = {collection.getDB.requestDone}

  def requestStart = {collection.getDB.requestStart}

  def insert(objects: List[JObject])      = collection.insert(objects.map(jObject2MongoObject(_)))

  def remove(filter: Option[MongoFilter]) = collection.remove(toMongoFilter(filter))

  def count(filter: Option[MongoFilter])  = collection.getCount(toMongoFilter(filter))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean) = collection.update(toMongoFilter(filter), value.toJValue, upsert, multi)

  def ensureIndex(name: String, keysPaths: List[JPath], unique: Boolean) = {
    val options = JObject(JField("name", JString(name)) :: JField("background", JBool(true)) :: JField("unique", JBool(unique)) :: Nil)

    collection.ensureIndex(toMongoKeys(keysPaths), options)
  }

  def dropIndex(name: String) = collection.dropIndex(name)

  def dropIndexes = collection.dropIndexes()

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {

    val sortObject   = sort.map(v => JObject(JField(JPathExtension.toMongoField(v.sortField), JInt(v.sortOrder.order)) :: Nil)).map(jObject2MongoObject(_))

    val cursor        = collection.find(toMongoFilter(filter), toMongoKeys(selection))
    val sortedCursor  = sortObject.map(cursor.sort(_)).getOrElse(cursor)
    val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
    val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)

    iterator(limitedCursor.iterator)
  }

  private def iterator(dbObjectsIterator: java.util.Iterator[com.mongodb.DBObject]): scala.collection.IterableView[JObject, Iterator[JObject]] = {
    val jObjectIterator = new Iterator[JObject]{
      def next()  = mongoObject2JObject(dbObjectsIterator.next)
      def hasNext = dbObjectsIterator.hasNext
    }

    new IterableViewImpl[JObject](jObjectIterator)
  }

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray = {
    val result = collection.group(toMongoKeys(selection), toMongoFilter(filter), initial, reduce)

    JArray(mongoObject2JObject(result.asInstanceOf[DBObject]).fields.map(_.value))
  }

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = {
    new RealMapReduceOutput(collection.mapReduce(map, reduce, outputCollection.getOrElse(null), toMongoFilter(filter)))
  }

  def distinct(selection: JPath, filter: Option[MongoFilter]) = {
    val key    = JPathExtension.toMongoField(selection)
    val result = filter.map(v => collection.distinct(key, v.filter.asInstanceOf[JObject])).getOrElse(collection.distinct(key))

    mongoObject2JObject(result.asInstanceOf[DBObject]).fields.map(_.value)
  }

  def getLastError: Option[BasicDBObject] = {
      val error  = collection.getDB.getLastError
      if (error != null && error.get("err") != null) Some(error) else None
  }

  private def toMongoKeys(selection : MongoSelection):    JObject = toMongoKeys(selection.selection)
  private def toMongoKeys(keysPaths: List[JPath]):        JObject = JObject(keysPaths.map(key => JField(JPathExtension.toMongoField(key), JInt(1))))
  private def toMongoFilter(filter: Option[MongoFilter]): JObject = jObject2MongoObject(filter.map(_.filter.asInstanceOf[JObject]).getOrElse(JObject(Nil)))
}

import com.mongodb.{MapReduceOutput => MongoMapReduceOutput}
private[mongo] class RealMapReduceOutput(output: MongoMapReduceOutput) extends MapReduceOutput{
  def drop = {output.drop}

  def outpotCollection = MongoCollectionHolder(new RealDatabaseCollection(output.getOutputCollection))
}

class IterableViewImpl[+A](underluyingIterator: scala.collection.Iterator[A]) extends scala.collection.IterableView[A, Iterator[A]]{
  def iterator: scala.collection.Iterator[A] = underluyingIterator

  protected def underlying = underluyingIterator
}


