package blueeyes.persistence.mongo

import blueeyes.bkka.Stop
import blueeyes.bkka.ActorRefStop
import blueeyes.bkka.AkkaDefaults
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import blueeyes.persistence.mongo.json.BijectionsMongoJson._
import blueeyes.persistence.mongo.json.BijectionsMongoJson.MongoToJson._
import IterableViewImpl._

import com.mongodb._
import org.streum.configrity.Configuration
import scala.collection.JavaConversions._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.ActorKilledException
import akka.actor.Props
import akka.actor.PoisonPill
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.MessageDispatcher
import akka.pattern.ask
import akka.routing.RoundRobinRouter
import akka.util.Duration
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scalaz.Scalaz._
import scalaz._
import com.weiglewilczek.slf4s.Logging

object RealMongo {
  val ServerAndPortPattern = "(.+):(.+)".r

  def apply(config: Configuration) = {
    val mongo = {
      val options = new MongoOptions()
      options.connectionsPerHost = 256
      options.threadsAllowedToBlockForConnectionMultiplier = 16

      val servers = config[List[String]]("servers").toList map {
        case ServerAndPortPattern(host, port) => new ServerAddress(host.trim(), port.trim().toInt)
        case server                           => new ServerAddress(server, ServerAddress.defaultPort())
      }

      val underlying = servers match {
        case x :: Nil => new com.mongodb.Mongo(x, options)
        case x :: xs  => new com.mongodb.Mongo(servers, options)
        case Nil => sys.error("""MongoServers are not configured. Configure the value 'servers'. Format is '["host1:port1", "host2:port2", ...]'""")
      }

      if (config[Boolean]("slaveOk", true)) { underlying.setReadPreference(ReadPreference.SECONDARY) }

      underlying
    }

    val disconnectTimeout = config.get[Long]("disconnect_timeout").getOrElse(300000L)

    new RealMongo(mongo, disconnectTimeout)
  }
}

class RealMongo(mongo: com.mongodb.Mongo, disconnectTimeout: Timeout) extends Mongo with AkkaDefaults {
  def database(databaseName: String) = new RealDatabase(this, mongo.getDB(databaseName), disconnectTimeout)
  lazy val close = Future(mongo.close)
}

object RealDatabase {
  //val actorSystem = ActorSystem.create("blueeyes_mongo") //TODO: separate mongo actor system creation out.
  private val actorSystem = ActorSystem("blueeyes_mongo")

  private class RealMongoActor extends Actor {
    def receive = {
      case MongoQueryTask(query, collection, isVerified) =>
        try {
          sender ! query(collection, isVerified)
        }
        catch {
          case e => sender ! akka.actor.Status.Failure(e)
        }
    }
  }
}

private[mongo] class RealDatabase(val mongo: Mongo, database: DB, disconnectTimeout: Timeout, poolSize: Int = 10) extends Database with Logging {
  import RealDatabase._

  private lazy val mongoActor =  actorSystem.actorOf(Props(() => new RealMongoActor).withRouter(RoundRobinRouter(poolSize).withDispatcher("blueeyes_mongo_" + database.getName)))

  protected def collection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName), this)

  def collections = database.getCollectionNames.map(collection).map(mc => MongoCollectionHolder(mc, mc.collection.getName, this)).toSet

  lazy val disconnect = ActorRefStop(actorSystem, disconnectTimeout).stop(mongoActor)

  protected def applyQuery[T <: MongoQuery](query: T, isVerified: Boolean)(implicit m: Manifest[T#QueryResult], queryTimeout: Timeout): Future[T#QueryResult]  =
    (mongoActor ? MongoQueryTask(query, query.collection, isVerified)).mapTo[T#QueryResult]

  override def toString = "Mongo Database: " + database.getName
}

private[mongo] class RealDatabaseCollection(val collection: DBCollection, database: RealDatabase) extends DatabaseCollection{
  type V[B] = ValidationNEL[String, B]

  def requestDone() { collection.getDB.requestDone() }

  def requestStart() { collection.getDB.requestStart() }

  def insert(objects: List[JObject]) = {
    objects.map(MongoToJson.unapply(_)).sequence[V, DBObject].map{objects: List[DBObject] =>
      collection.insert(objects)
    }
  }

  def remove(filter: Option[MongoFilter]) { collection.remove(toMongoFilter(filter)) }

  def count(filter: Option[MongoFilter])  = collection.getCount(toMongoFilter(filter))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean) {
    collection.update(toMongoFilter(filter), value.toJValue, upsert, multi) }

  def ensureIndex(name: String, keysPaths: Seq[(JPath, IndexType)], unique: Boolean, options: JObject) {
    val indexOptions = JObject(
      JField("name", JString(name)) ::
      JField("background", JBool(true)) ::
      JField("unique", JBool(unique)) :: options.fields
    )

    def toMongoIndexType(indexType: IndexType) = {
      indexType match{
        case OrdinaryIndex   => JInt(1)
        case GeospatialIndex => JString("2d")
      }
    }
    val indexKeys = JObject(keysPaths.distinct.map(key => JField(JPathExtension.toMongoField(key._1), toMongoIndexType(key._2))).toList)

    collection.ensureIndex(indexKeys, indexOptions)
  }

  def dropIndex(name: String) { collection.dropIndex(name) }

  def dropIndexes() { collection.dropIndexes() }

  def explain(selection: MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean): JObject =
    dbo2jvo(find(selection, filter, sort, skip, limit, hint, isSnapshot).explain())

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean) =
    iterator(find(selection, filter, sort, skip, limit, hint, isSnapshot).iterator)

  private def iterator(dbObjectsIterator: java.util.Iterator[com.mongodb.DBObject]): scala.collection.IterableView[JObject, Iterator[JObject]] = {
    val jObjectIterator = new Iterator[JObject]{
      def next()  = MongoToJson(dbObjectsIterator.next)
      def hasNext = dbObjectsIterator.hasNext
    }

    new IterableViewImpl[JObject, Iterator[JObject]](jObjectIterator)
  }

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray = {
    val result = collection.group(toMongoKeys(selection), toMongoFilter(filter), initial, reduce)

    JArray(MongoToJson(result.asInstanceOf[DBObject]).fields.map(_.value))
  }

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = {
    new RealMapReduceOutput(collection.mapReduce(map, reduce, outputCollection.getOrElse(null), toMongoFilter(filter)), database)
  }

  def distinct(selection: JPath, filter: Option[MongoFilter]) = {
    val key    = JPathExtension.toMongoField(selection)
    val result = filter.map(v => collection.distinct(key, v.filter.asInstanceOf[JObject])).getOrElse(collection.distinct(key))

    MongoToJson(result.asInstanceOf[DBObject]).fields.map(_.value)
  }

  def getLastError: Option[BasicDBObject] = {
    val error  = collection.getDB.getLastError
    if (error != null && error.get("err") != null) Some(error) else None
  }

  def selectAndUpdate(filter: Option[MongoFilter], sort: Option[MongoSort], value: MongoUpdate, selection: MongoSelection, returnNew: Boolean, upsert: Boolean) =
    selectAndUpdate(filter, sort, value, selection, false, returnNew, upsert)

  def selectAndRemove(filter: Option[MongoFilter], sort: Option[MongoSort], selection: MongoSelection) =
    selectAndUpdate(filter, sort, MongoUpdateNothing, selection, true, false, false)

  private def selectAndUpdate(filter: Option[MongoFilter], sort: Option[MongoSort], value: MongoUpdate, selection: MongoSelection, remove: Boolean, returnNew: Boolean, upsert: Boolean) =
    Option(collection.findAndModify(toMongoFilter(filter), toMongoKeys(selection), toMongoSort2(sort), remove, value.toJValue, returnNew, upsert)).map(dbo2jvo(_))


  private def find(selection: MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean): DBCursor = {
    val cursor        = collection.find(toMongoFilter(filter), toMongoKeys(selection))
    val sortedCursor  = toMongoSort(sort).map(cursor.sort(_)).getOrElse(cursor)
    val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
    val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)
    val hintedCursor  = hint.map{value => value match{
      case NamedHint(name) => limitedCursor.hint(name)
      case KeyedHint(keys) => limitedCursor.hint(toMongoKeys(keys.distinct))
    }}.getOrElse(limitedCursor)
    if (isSnapshot) hintedCursor.snapshot() else hintedCursor
  }

  private def toMongoSort(sort: Option[MongoSort]): Option[DBObject] = sort.map(toMongoSort(_)) map { jvo2dbo(_) }
  private def toMongoSort2(sort: Option[MongoSort]): DBObject = jvo2dbo(sort.map(toMongoSort(_)).getOrElse(JObject(Nil)))
  private def toMongoSort(sort: MongoSort) = JObject(JField(JPathExtension.toMongoField(sort.sortField), JInt(sort.sortOrder.order)) :: Nil)
  private def toMongoKeys(selection : MongoSelection): JObject = toMongoKeys(selection.selection)
  private def toMongoKeys(keysPaths: Iterable[JPath]): JObject = JObject(keysPaths.map(key => JField(JPathExtension.toMongoField(key), JInt(1))).toList)
  private def toMongoFilter(filter: Option[MongoFilter])       = filter.map(_.filter.asInstanceOf[JObject]).getOrElse(JObject(Nil))

  private implicit def unvalidated(v: ValidationNEL[String, JObject]): JObject = v ||| {
    errors => sys.error("An error occurred deserializing the database object: " + errors.list.mkString("; "))
  }

  private implicit def jvo2dbo(obj: JObject): DBObject = MongoToJson.unapply(obj) ||| {
    errors => sys.error("An error occurred serializing the JSON object to mongo: " + errors.list.mkString("; "))
  }

  private def dbo2jvo(dbo: DBObject): JObject = MongoToJson(dbo) ||| {
    errors => sys.error("Errors occurred deserializing the explain object: " + errors.list.mkString("; "))
  }
}

import com.mongodb.{MapReduceOutput => MongoMapReduceOutput}
private[mongo] class RealMapReduceOutput(output: MongoMapReduceOutput, database: RealDatabase) extends MapReduceOutput{
  override def outputCollection = MongoCollectionHolder(new RealDatabaseCollection(output.getOutputCollection, database), output.getOutputCollection.getName, database)
  def drop() { output.drop() }
}

import scala.collection.Iterator
object IterableViewImpl {
  implicit def iteratorToIterator[A](iterator: Iterator[A]): Iterator[A] = iterator
  implicit def seqToIterator[A](seq: Seq[A]): Iterator[A] = seq.iterator
}

class IterableViewImpl[+A, +Coll](delegate: Coll)(implicit f: Coll => Iterator[A]) extends scala.collection.IterableView[A, Coll]{
  def iterator: Iterator[A] = f(delegate)

  protected def underlying = delegate
}
