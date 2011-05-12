package blueeyes.persistence.mongo

import scala.collection.IterableView
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import com.mongodb.MongoException
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
 * database(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" <<))
 * </pre
 *
 * To to know whether or not operation succeeded, or if it did not succeed, what error it generated it is necessary
 * to create "verified" query:
 *
 * val query =  verified(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" <<))
 */
abstract class MongoDatabase(implicit executionStrategy: ActorExecutionStrategy, deliveryStrategy: FutureDeliveryStrategy){
  private lazy val mongoActor = new Actor{

    val query = lift2((query: MongoQuery[_], collection: DatabaseCollection) => query(collection))
  }

  def apply[T](query: MongoQuery[T]): Future[T]  = {
    val databaseCollection = query.collection match{
      case MongoCollectionReference(name)         => collection(name)
      case MongoCollectionHolder(realCollection)  => realCollection
    }
    mongoActor.query(query, databaseCollection).asInstanceOf[Future[T]]
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
  def ensureIndex(name: String, keys: List[JPath], unique: Boolean)
  def dropIndexes
  def dropIndex(name: String)
  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean)
  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter] = None): MapReduceOutput
  def requestStart: Unit
  def requestDone: Unit
  def getLastError: Option[com.mongodb.BasicDBObject]
}