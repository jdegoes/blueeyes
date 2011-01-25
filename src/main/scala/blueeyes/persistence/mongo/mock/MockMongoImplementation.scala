package blueeyes.persistence.mongo.mock

import blueeyes.json.JsonAST._
import blueeyes.json.{JPath}
import blueeyes.persistence.mongo._
import blueeyes.persistence.mongo.MongoFilterEvaluator._

@com.google.inject.Singleton
class MockMongo() extends Mongo{
  private val databases     = scala.collection.mutable.Map[String, MockMongoDatabase]()
  def database(databaseName: String) = {
    databases.get(databaseName) match{
      case Some(x) => x
      case None =>{
        val mongoDatabase  = new MockMongoDatabase()
        databases.put(databaseName, mongoDatabase)
        mongoDatabase
      }
    }
  }
}

private[mongo] class MockMongoDatabase() extends MongoDatabase{
  private val collections   = scala.collection.mutable.Map[String, MockDatabaseCollection]()

  def collection(collectionName: String) = {
    collections.get(collectionName) match{
      case Some(x) => x
      case None =>{
        val collection  = new MockDatabaseCollection()
        collections.put(collectionName, collection)
        collection
      }
    }
  }
}

private[mongo] class MockDatabaseCollection() extends DatabaseCollection with JObjectFields with MockIndex{
  private var container = JArray(Nil)

  def insert(objects: List[JObject]): Unit = {
    index(objects)
    insert0(objects)
  }

  def remove(filter: Option[MongoFilter]) : Unit = remove(search(filter))

  def count(filter: Option[MongoFilter]) = search(filter).size

  def indexed = all

  def distinct(selection: JPath, filter: Option[MongoFilter]) =
    search(filter).map(jobject => selectByPath(selection, jobject, (v) => {Some(v)}, (p, v) => {v})).filter(_.isDefined).map(_.get).distinct

  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String) = GroupFunction(selection, initial, reduce, search(filter))

  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter]) = MapReduceFunction(map, reduce, outputCollection, search(filter))  

  private def insert0(objects: List[JObject]) = container = JArray(container.elements ++ objects)

  private def search(filter: Option[MongoFilter]): List[JObject] = filter.map(all.filter(_).map(_.asInstanceOf[JObject])).getOrElse(all)

  private def all: List[JObject] = container.elements.map(_.asInstanceOf[JObject])

  private def remove(objects: List[JObject]): Unit = container = JArray(all filterNot (objects contains))

  def update(filter: Option[MongoFilter], value : MongoUpdate, upsert: Boolean, multi: Boolean){
    var (objects, update) = (if (multi) search(filter) else search(filter).headOption.map(_ :: Nil).getOrElse(Nil)) match {
                              case Nil if upsert => (List(JObject(Nil)), value & filter.map(FilterToUpdateConvert(_)).getOrElse(MongoUpdateNothing))
                              case v => (v, value)
                            }
    var updated = UpdateFunction(update, objects)

    if (upsert) remove(objects)

    index(updated)
    remove(objects)
    insert0(updated)
  }

  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]) = {
    val objects = search(filter)
    val sorted  = sort.map(v => objects.sorted(new JObjectOrdering(v.sortField, v.sortOrder.order))).getOrElse(objects)
    val skipped = skip.map(sorted.drop(_)).getOrElse(sorted)
    val limited = limit.map(skipped.take(_)).getOrElse(skipped)

    selectExistingFields(limited, selection.selection).map(_.asInstanceOf[JObject]).toStream
  }
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
    case x : MongoAndFilter           => x.queries.foldLeft(MongoUpdateNothing.asInstanceOf[MongoUpdate]){ (result, currentFilter) =>  result &  FilterToUpdateConvert(currentFilter)}
  }
}

private[mongo] class MockMapReduceOutput(output: MockDatabaseCollection) extends MapReduceOutput{
  def drop = {}

  def outpotCollection = MongoCollectionHolder(output)
}
