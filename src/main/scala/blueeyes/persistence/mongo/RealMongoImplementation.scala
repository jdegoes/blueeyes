package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.json.{JPath}
import blueeyes.persistence.mongo.json.MongoJsonBijection
import blueeyes.persistence.mongo.json.MongoJsonBijection._
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._
import IterableViewImpl._

import com.mongodb._
import net.lag.configgy.ConfigMap
import scala.collection.JavaConversions._
import scala.collection.immutable.ListSet

import akka.actor.Actor._
import akka.actor.Actor
import akka.routing.Routing._
import akka.routing.CyclicIterator
import akka.dispatch.Dispatchers
import akka.util.Duration

import java.util.concurrent.TimeUnit

class RealMongo(config: ConfigMap) extends Mongo {
  val ServerAndPortPattern = "(.+):(.+)".r

  private lazy val mongo = {
    val options = new MongoOptions()
    options.connectionsPerHost = 1000
    options.threadsAllowedToBlockForConnectionMultiplier = 1000

    val servers = config.getList("servers").map(server =>{
      server match{
        case ServerAndPortPattern(host, port) => new ServerAddress(host.trim(), port.trim().toInt)
        case _ => new ServerAddress(server, ServerAddress.defaultPort())
      }
    }).toList

    val mongo = servers match {
      case x :: Nil => new com.mongodb.Mongo(x, options)
      case x :: xs  => new com.mongodb.Mongo(servers, options)
      case Nil => sys.error("""MongoServers are not configured. Configure the value 'servers'. Format is '["host1:port1", "host2:port2", ...]'""")
    }

    if (config.getBool("slaveOk", true)) { mongo.slaveOk() }

    mongo
  }

  def database(databaseName: String) = new RealDatabase(this, mongo.getDB(databaseName))
}

object RealMongoActor {
  val dispatcher = Dispatchers.newExecutorBasedEventDrivenDispatcher("blueeyes_mongo")
      .withNewThreadPoolWithLinkedBlockingQueueWithUnboundedCapacity.setCorePoolSize(8)
      .setMaxPoolSize(100).setKeepAliveTime(Duration(30, TimeUnit.SECONDS)).build
}

class RealMongoActor extends Actor {
  self.dispatcher = RealMongoActor.dispatcher
  def receive = {
    case task: MongoQueryTask => self.reply(task.query(task.collection, task.isVerified))
    case _ => sys.error("wrong message.")
  }
}

private[mongo] class RealDatabase(val mongo: Mongo, database: DB) extends Database {
  private val poolSize = 10

  private lazy val actors     = List.fill(poolSize)(Actor.actorOf[RealMongoActor].start)
  private lazy val mongoActor = loadBalancerActor( new CyclicIterator( actors ) )

  protected def collection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName), this)

  def collections = database.getCollectionNames.map(collection).map(mc => MongoCollectionHolder(mc, mc.collection.getName, this)).toSet

  def disconnect() = {
    actors.foreach(_.stop())
    mongoActor.stop
  }

  protected def applyQuery[T <: MongoQuery](query: T, isVerified: Boolean): Future[T#QueryResult]  =
//    Future.sync(query(query.collection, isVerified))
    mongoActor.!!![T#QueryResult](MongoQueryTask(query, query.collection, isVerified), 1000 * 60 * 60).toBlueEyes
}

private[mongo] class RealDatabaseCollection(val collection: DBCollection, database: RealDatabase) extends DatabaseCollection{
  def requestDone = collection.getDB.requestDone

  def requestStart = collection.getDB.requestStart

  def insert(objects: List[JObject])      = collection.insert(objects.map(MongoJsonBijection.unapply(_)))

  def remove(filter: Option[MongoFilter]) = collection.remove(toMongoFilter(filter))

  def count(filter: Option[MongoFilter])  = collection.getCount(toMongoFilter(filter))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean) = 
    collection.update(toMongoFilter(filter), value.toJValue, upsert, multi)

  def ensureIndex(name: String, keysPaths: ListSet[JPath], unique: Boolean) = {
    val options = JObject(
      JField("name", JString(name)) :: 
      JField("background", JBool(true)) :: 
      JField("unique", JBool(unique)) :: Nil
    )

    collection.ensureIndex(toMongoKeys(keysPaths), options)
  }

  def dropIndex(name: String) = collection.dropIndex(name)

  def dropIndexes = collection.dropIndexes()

  def explain(selection: MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean): JObject =
    find(selection, filter, sort, skip, limit, hint, isSnapshot).explain()

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean) =
    iterator(find(selection, filter, sort, skip, limit, hint, isSnapshot).iterator)

  private def iterator(dbObjectsIterator: java.util.Iterator[com.mongodb.DBObject]): scala.collection.IterableView[JObject, Iterator[JObject]] = {
    val jObjectIterator = new Iterator[JObject]{
      def next()  = MongoJsonBijection.apply(dbObjectsIterator.next)
      def hasNext = dbObjectsIterator.hasNext
    }

    new IterableViewImpl[JObject, Iterator[JObject]](jObjectIterator)
  }

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray = {
    val result = collection.group(toMongoKeys(selection), toMongoFilter(filter), initial, reduce)

    JArray(MongoJsonBijection.apply(result.asInstanceOf[DBObject]).fields.map(_.value))
  }

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = {
    new RealMapReduceOutput(collection.mapReduce(map, reduce, outputCollection.getOrElse(null), toMongoFilter(filter)), database)
  }

  def distinct(selection: JPath, filter: Option[MongoFilter]) = {
    val key    = JPathExtension.toMongoField(selection)
    val result = filter.map(v => collection.distinct(key, v.filter.asInstanceOf[JObject])).getOrElse(collection.distinct(key))

    MongoJsonBijection.apply(result.asInstanceOf[DBObject]).fields.map(_.value)
  }

  def getLastError: Option[BasicDBObject] = {
      val error  = collection.getDB.getLastError
      if (error != null && error.get("err") != null) Some(error) else None
  }

  private def find(selection: MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean): DBCursor = {
    val sortObject   = sort.map(v => JObject(JField(JPathExtension.toMongoField(v.sortField), JInt(v.sortOrder.order)) :: Nil)).map(MongoJsonBijection.unapply(_))

    val cursor        = collection.find(toMongoFilter(filter), toMongoKeys(selection))
    val sortedCursor  = sortObject.map(cursor.sort(_)).getOrElse(cursor)
    val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
    val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)
    val hintedCursor  = hint.map{value => value match{
      case NamedHint(name) => limitedCursor.hint(name)
      case KeyedHint(keys) => limitedCursor.hint(toMongoKeys(keys))
    }}.getOrElse(limitedCursor)
    if (isSnapshot) hintedCursor.snapshot() else hintedCursor
  }

  private def toMongoKeys(selection : MongoSelection): JObject = toMongoKeys(selection.selection)
  private def toMongoKeys(keysPaths: Set[JPath]): JObject      = JObject(keysPaths.toList.map(key => JField(JPathExtension.toMongoField(key), JInt(1))))
  private def toMongoFilter(filter: Option[MongoFilter])       = filter.map(_.filter.asInstanceOf[JObject]).getOrElse(JObject(Nil))
}

import com.mongodb.{MapReduceOutput => MongoMapReduceOutput}
private[mongo] class RealMapReduceOutput(output: MongoMapReduceOutput, database: RealDatabase) extends MapReduceOutput{
  override def outputCollection = MongoCollectionHolder(new RealDatabaseCollection(output.getOutputCollection, database), output.getOutputCollection.getName, database)
  def drop = output.drop
}

import scala.collection.Iterator
object IterableViewImpl{
  implicit def iteratorToIterator[A](iterator: Iterator[A]): Iterator[A] = iterator
  implicit def seqToIterator[A](seq: Seq[A]): Iterator[A] = seq.iterator
}
class IterableViewImpl[+A, +Coll](delegate: Coll)(implicit f: Coll => Iterator[A]) extends scala.collection.IterableView[A, Coll]{
  def iterator: Iterator[A] = delegate

  protected def underlying = delegate
}


