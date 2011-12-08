package blueeyes.persistence.mongo

import scala.collection.IterableView
import blueeyes.bkka.Stop
import blueeyes.json.JPath
import blueeyes.json.JsonAST._
import blueeyes.json.{Printer, JsonAST}
import blueeyes.concurrent.Future
import blueeyes.concurrent.Future._

/** The Mongo creates a MongoDatabase by  database name.
 * <p>
 */
trait Mongo {
  def database(databaseName: String): Database
  def close: akka.dispatch.Future[Unit]
}

object Mongo {
  implicit def stop: Stop[Mongo] = new Stop[Mongo] {
    def stop(mongo: Mongo) = mongo.close
  }
}

case class MongoQueryTask(query: MongoQuery, collection: DatabaseCollection, isVerified: Boolean)

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
abstract class Database {
  def mongo: Mongo

  def apply[T <: MongoQuery](query: T)(implicit m: Manifest[T#QueryResult]): Future[T#QueryResult]  = applyQuery(query, true)

  def unverified[T <: MongoQuery](query: T)(implicit m: Manifest[T#QueryResult]): Future[T#QueryResult]  = applyQuery(query, false)

  def collections: Set[MongoCollectionHolder]

  def dump(print: String => Unit = (value: String) => print(value))  = {
    collections foreach { mongoCollection =>
      print("""{
  "%s":[""".format(mongoCollection.name))

      val jobjects = collection(mongoCollection.name).select(MongoSelection(Set()), None, None, None, None, None, false)
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

  def disconnect: akka.dispatch.Future[Any]

  implicit protected def databaseCollection(mongoCollection: MongoCollection) = mongoCollection match{
    case MongoCollectionReference(name)         => collection(name)
    case MongoCollectionHolder(realCollection, name, database)  => realCollection
  }

  protected def applyQuery[T <: MongoQuery](query: T, isVerified: Boolean)(implicit m: Manifest[T#QueryResult]): Future[T#QueryResult]

  protected def collection(collectionName: String): DatabaseCollection
}

object Database {
  implicit def stop: Stop[Database] = new Stop[Database] {
    def stop(db: Database) = db.disconnect
  }
}

private[mongo] trait DatabaseCollection{
  def insert(objects: List[JObject])//: ValidationNEL[String, Unit]
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int],
             limit: Option[Int], hint: Option[Hint], isSnapshot : Boolean): IterableView[JObject, Iterator[JObject]]
  def explain(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int],
              limit: Option[Int], hint: Option[Hint], isSnapshot : Boolean): JObject
  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray
  def distinct(selection : JPath, filter: Option[MongoFilter]): List[JValue]
  def remove(filter: Option[MongoFilter])
  def count(filter: Option[MongoFilter]): Long
  def ensureIndex(name: String, keys: Seq[(JPath, IndexType)], unique: Boolean, options: JObject)
  def dropIndexes()
  def dropIndex(name: String)
  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean)
  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter] = None): MapReduceOutput
  def requestStart(): Unit
  def requestDone(): Unit
  def getLastError: Option[com.mongodb.BasicDBObject]
  def selectAndUpdate(filter: Option[MongoFilter], sort: Option[MongoSort], value: MongoUpdate,
                      selection: MongoSelection, returnNew: Boolean, upsert: Boolean): Option[JObject]
  def selectAndRemove(filter: Option[MongoFilter], sort: Option[MongoSort], selection: MongoSelection): Option[JObject]
}
