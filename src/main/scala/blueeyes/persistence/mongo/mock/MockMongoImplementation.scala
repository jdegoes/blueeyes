package blueeyes.persistence.mongo.mock

import blueeyes.concurrent.ReadWriteLock
import blueeyes.json.JsonAST._
import blueeyes.json.{JPath, JsonParser}
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.IterableViewImpl._
import blueeyes.persistence.mongo.MongoFilterEvaluator._
import collection.mutable.ConcurrentMap
import java.util.concurrent.ConcurrentHashMap
import scala.collection.JavaConversions._
import blueeyes.concurrent.Future
import scalaz._
import Scalaz._
import com.mongodb.MongoException

class MockMongo() extends Mongo {
  private val databases: ConcurrentMap[String, MockDatabase]     = new ConcurrentHashMap[String, MockDatabase]()
  def database(databaseName: String) = {
    databases.get(databaseName).getOrElse({
      val mongoDatabase  = new MockDatabase(this)
      databases.putIfAbsent(databaseName, mongoDatabase).getOrElse(mongoDatabase)
    })
  }
}

private[mongo] class MockDatabase(val mongo: Mongo) extends Database {
  private val databaseCollections: ConcurrentMap[String, MockDatabaseCollection]   = new ConcurrentHashMap[String, MockDatabaseCollection]()

  def collection(collectionName: String) = {
    databaseCollections.get(collectionName).getOrElse({
      val collection  = new MockDatabaseCollection(collectionName, this)
      databaseCollections.putIfAbsent(collectionName, collection).getOrElse(collection)
    })
  }

  def applyQuery[T <: MongoQuery](query: T, isVerified: Boolean)(implicit m: Manifest[T#QueryResult]) = Future.sync(query(query.collection, isVerified))

  def collections = databaseCollections.entrySet.map(entry => MongoCollectionHolder(entry.getValue, entry.getKey, this)).toSet

  def disconnect(timeout: Long) = akka.dispatch.Future.empty[Any]()
}

private[mongo] class MockDatabaseCollection(val name: String, val database: MockDatabase) extends DatabaseCollection with JObjectFields with MockIndex with ReadWriteLock with MongoFilters {
  private var container = JArray(Nil)
  private val explanation = JsonParser.parse("""{
        "cursor" : "BasicCursor",
        "nscanned" : 3,
        "nscannedObjects" : 3,
        "n" : 3,
        "millis" : 38,
        "nYields" : 0,
        "nChunkSkips" : 0,
        "isMultiKey" : false,
        "indexOnly" : false,
        "indexBounds" : {

        }
}""").asInstanceOf[JObject]

  def insert(objects: List[JObject]) {
    writeLock{
      index(objects)
      insert0(objects)
    }
  }

  def count(filter: Option[MongoFilter]) = safeProcess(filter, {found: List[JObject] => found.size})

  def distinct(selection: JPath, filter: Option[MongoFilter]) =
    safeProcess(filter, (_: List[JObject]).flatMap(selectByPath(selection, _, Some(_), (p, v) => v)).distinct)

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String) =
    safeProcess(filter, GroupFunction(selection, initial, reduce, _))

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) =
    safeProcess(filter, MapReduceFunction(map, reduce, outputCollection, _, database))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean) { selectAndUpdate(filter, None, value, MongoSelection(Set()), true, upsert, multi) }

  def selectAndUpdate(filter: Option[MongoFilter], sort: Option[MongoSort], value: MongoUpdate, selection: MongoSelection, returnNew: Boolean, upsert: Boolean) =
    selectAndUpdate(filter, sort, value, selection, returnNew, upsert, false).headOption

  private def selectAndUpdate(filter: Option[MongoFilter], sort: Option[MongoSort], value: MongoUpdate, selection: MongoSelection,
                      returnNew: Boolean, upsert: Boolean, multi: Boolean) = {
    writeLock{
      val selected          = select(MongoSelection(Set()), filter, sort, None, None, None, false).toList
      var (objects, update) = (if (multi) selected else selected.headOption.map(_ :: Nil).getOrElse(Nil)) match {
                                case Nil if upsert => value match {
                                  case e: MongoUpdateObject => (List(JObject(Nil)), value)
                                  case _ => (List(JObject(Nil)), value |+| filter.map(FilterToUpdateConvert(_)).getOrElse(MongoUpdateNothing))
                                }
                                case v => (v, value)
                              }
      var updated = UpdateFunction(update, objects)

      if (upsert) remove(objects)

      index(updated)
      remove(objects)
      insert0(updated)

      val toReturn = if (returnNew) updated else objects.filter(_ != JObject(Nil))
      selectExistingFields(toReturn, selection.selection).collect{case jvo: JObject => jvo}
    }
  }

  def remove(filter: Option[MongoFilter]) { selectAndRemove(filter, None, MongoSelection(Set()), true) }

  def selectAndRemove(filter: Option[MongoFilter], sort: Option[MongoSort], selection: MongoSelection) =
    selectAndRemove(filter, sort, selection, false)

  private def selectAndRemove(filter: Option[MongoFilter], sort: Option[MongoSort], selection: MongoSelection, multi: Boolean) = {
    writeLock{
      val selected = select(MongoSelection(Set()), filter, sort, None, None, None, false).toList
      val jvos     = if (multi) selected else selected.headOption.toList

      remove(jvos)
      selectExistingFields(jvos, selection.selection).collect{case jvo: JObject => jvo}.headOption
    }
  }

  def explain(selection: MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean) = explanation

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int], hint: Option[Hint], isSnapshot: Boolean) = {
    safeProcess(filter, {objects: List[JObject] =>
      if (isSnapshot && (sort.isDefined || hint.isDefined)) throw new MongoException("Hint and sorting cannot be used with $snapshot")
      checkHint(hint)

      val sorted    = JObjectOrderingFactory(filter, sort).map(sorting => objects.sorted(sorting)).getOrElse(objects)
      val skipped   = skip.map(sorted.drop(_)).getOrElse(sorted)
      val realLimit = limit.orElse(filter.flatMap(nearFilter(_)).map(v => 100))
      val limited   = realLimit.map(skipped.take(_)).getOrElse(skipped)

      val found  = selectExistingFields(limited, selection.selection).collect{case jvo: JObject => jvo}
      val result = if (isSnapshot) found.distinct else found

      new IterableViewImpl[JObject, Iterator[JObject]](result.iterator)
    })
  }
  private def checkHint(hint: Option[Hint]) = hint match{
    case Some(NamedHint(name)) => if (!indexExists(name)) throw new MongoException("bad index")
    case Some(KeyedHint(keys)) => if (!indexExists(keys)) throw new MongoException("bad index")
    case _ =>
  }

  def requestStart() {}

  def requestDone() {}

  def getLastError: Option[com.mongodb.BasicDBObject] = None

  override def ensureIndex(name: String, keys: Seq[(JPath, IndexType)], unique: Boolean, options: JObject) {
    writeLock {
      super.ensureIndex(name, keys.distinct, unique, options)
    }
  }

  override def dropIndexes() {
    writeLock{
      super.dropIndexes()
    }
  }

  override def dropIndex(name: String){
    writeLock{
      super.dropIndex(name)
    }
  }

  def indexed = all

  private def safeProcess[T](filter: Option[MongoFilter], f: (List[JObject]) => T): T = {
    readLock{
      process(filter, f)
    }
  }
  private def process[T](filter: Option[MongoFilter], f: (List[JObject]) => T): T = f(search(filter))

  private def search(filter: Option[MongoFilter]) = filter.map(all.filter(_).map(_.asInstanceOf[JObject])).getOrElse(all)

  private def insert0(objects: List[JObject]) {container = JArray(container.elements ++ objects)}

  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])

  private def remove(objects: List[JObject]) {container = JArray(all filterNot (objects contains))}
}

private[mock] object FilterToUpdateConvert{
  import MongoFilterOperators._

  def apply(filter: MongoFilter):  MongoUpdate = filter match{
    case MongoFilterAll               => MongoUpdateNothing
    case x : MongoOrFilter            => MongoUpdateNothing
    case x : MongoElementsMatchFilter => MongoUpdateNothing
    case x : MongoFieldFilter         => x.operator match {
      case $eq  => x.lhs set x.rhs
      case _    => MongoUpdateNothing
    }
    case x : MongoAndFilter           => x.queries.distinct.map(FilterToUpdateConvert(_)).asMA.sum
  }
}

private[mongo] class MockMapReduceOutput(output: MockDatabaseCollection) extends MapReduceOutput {
  override def outputCollection = MongoCollectionHolder(output, output.name, output.database)
  override def drop() {}
}
