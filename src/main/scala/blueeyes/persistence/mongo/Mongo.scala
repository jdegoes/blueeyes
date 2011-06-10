package blueeyes.persistence.mongo

import scala.collection.IterableView
import scala.collection.immutable.ListSet
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.json.{Printer, JsonAST}
import blueeyes.concurrent._

/** The Mongo creates a MongoDatabase by  database name.
 * <p>
 * <pre>
 * lazy val injector = Guice.createInjector(new FilesystemConfiggyModule(ConfiggyModule.FileLoc), new RealMongoModule)
 *
 * val mongo = injector.getInstance(classOf[Mongo])
 * </pre>
 */
trait Mongo{
  def database(databaseName: String): MongoDatabase
}

class MongoActor extends Actor {
  val query = lift3((query: MongoQuery[_], collection: DatabaseCollection, isVerified: Boolean) => query(collection, isVerified))
}

/** The MongoDatabase executes MongoQuery.
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo._
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val injector = Guice.createInjector(new FilesystemConfiggyModule(ConfiggyModule.FileLoc), new RealMongoModule)
 *
 * val mongo = injector.getInstance(classOf[Mongo])
 *
 * val database  = mongo.database( "mydb" )
 *
 * database(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" &lt;&lt;))
 * </pre>
 *
 * To to know whether or not operation succeeded, or if it did not succeed, what error it generated it is necessary
 * to create "verified" query:
 *
 * val query =  verified(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" &lt;&lt;))
 */
abstract class MongoDatabase(implicit executionStrategy: ActorExecutionStrategy, deliveryStrategy: FutureDeliveryStrategy){
  def mongo: Mongo

  private lazy val mongoActor = new MongoActor

  def apply[T](query: MongoQuery[T]): Future[T]  = applyQuery(query, true)

  def unverified[T](query: MongoQuery[T]): Future[T]  = applyQuery(query, false)

  private def applyQuery[T](query: MongoQuery[T], isVerified: Boolean): Future[T]  = {
    val databaseCollection = query.collection match{
      case MongoCollectionReference(name)         => collection(name)
      case MongoCollectionHolder(realCollection, name, database)  => realCollection
    }

    mongoActor.query(query, databaseCollection, isVerified).asInstanceOf[Future[T]]
  }

  def collections: Set[MongoCollectionHolder]

  def dump(print: String => Unit = (value: String) => print(value))  = {
    collections foreach { mongoCollection =>
      print("""{
  "%s":[""".format(mongoCollection.name))

      val jobjects = collection(mongoCollection.name).select(MongoSelection(Set()), None, None, None, None)
      var first    = true

      jobjects foreach { jobject =>
        val prefix = if (first) {
          first = false
          ""
        } else ","

        print(prefix + Printer.pretty(scala.text.DocNest(2, JsonAST.render(jobject))))
      }

      print("""]
}""")
    }
  }

  protected def collection(collectionName: String): DatabaseCollection
}

private[mongo] trait DatabaseCollection{
  def insert(objects: List[JObject])
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]): IterableView[JObject, Iterator[JObject]]
  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray
  def distinct(selection : JPath, filter: Option[MongoFilter]): List[JValue]
  def remove(filter: Option[MongoFilter])
  def count(filter: Option[MongoFilter]): Long
  def ensureIndex(name: String, keys: ListSet[JPath], unique: Boolean)
  def dropIndexes
  def dropIndex(name: String)
  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean)
  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter] = None): MapReduceOutput
  def requestStart: Unit
  def requestDone: Unit
  def getLastError: Option[com.mongodb.BasicDBObject]
}
