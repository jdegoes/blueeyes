package blueeyes.persistence.mongo

import scala.collection.IterableView
import scala.collection.immutable.ListSet
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.json.{Printer, JsonAST}
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

/** The Mongo creates a MongoDatabase by  database name.
 * <p>
 */
trait Mongo {
  def database(databaseName: String): MongoDatabase
}

case class MongoQueryTask(query: MongoQuery[_], collection: DatabaseCollection, isVerified: Boolean)

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
abstract class MongoDatabase {
  def mongo: Mongo

  def apply[T](query: MongoQuery[T]): Future[T]  = applyQuery(query, true)

  def unverified[T](query: MongoQuery[T]): Future[T]  = applyQuery(query, false)

  def collections: Set[MongoCollectionHolder]

  def dump(print: String => Unit = (value: String) => print(value))  = {
    collections foreach { mongoCollection =>
      print("""{
  "%s":[""".format(mongoCollection.name))

      val jobjects = collection(mongoCollection.name).select(MongoSelection(Set()), None, None, None, None, None)
      var first    = true

      jobjects foreach { jobject =>
        val prefix = if (first) {
          first = false
          ""
        } else ","

        print(prefix + Printer.pretty(scala.text.DocNest(2, Printer.render(jobject))))
      }

      print("""]
}""")
    }
  }

  implicit protected def databaseCollection(mongoCollection: MongoCollection) = mongoCollection match{
    case MongoCollectionReference(name)         => collection(name)
    case MongoCollectionHolder(realCollection, name, database)  => realCollection
  }

  protected def applyQuery[T](query: MongoQuery[T], isVerified: Boolean): Future[T]

  protected def disconnect()

  protected def collection(collectionName: String): DatabaseCollection
}

private[mongo] trait DatabaseCollection{
  def insert(objects: List[JObject])
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint]): IterableView[JObject, Iterator[JObject]]
  def explain(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint]): JObject
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
