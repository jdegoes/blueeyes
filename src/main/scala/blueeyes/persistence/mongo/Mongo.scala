package blueeyes.persistence.mongo

import blueeyes.json.JPath
import blueeyes.json.JsonAST._

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
 * val database  = mongo.database( "mydb" );
 *
 * database(selectOne().from("mycollection").where("foo.bar" === "blahblah").sortBy("foo.bar" <<))
 * </pre
 */
trait MongoDatabase{
  def apply[T](query: MongoQuery[T]): T  = {
    val databaseCollection = query.collection match{
      case MongoCollectionReference(name)         => collection(name)
      case MongoCollectionHolder(realCollection)  => realCollection
    }
    query(databaseCollection)
  }

  protected def collection(collectionName: String): DatabaseCollection
}

private[mongo] trait DatabaseCollection{
  def insert(objects: List[JObject])
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]): Stream[JObject]
  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray
  def distinct(selection : JPath, filter: Option[MongoFilter]): List[JValue]
  def remove(filter: Option[MongoFilter])
  def count(filter: Option[MongoFilter]): Long
  def ensureIndex(name: String, keys: List[JPath], unique: Boolean)
  def dropIndexes
  def dropIndex(name: String)
  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean)
  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter] = None): MapReduceOutput
}