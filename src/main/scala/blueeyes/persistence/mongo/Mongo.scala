package blueeyes.persistence.mongo

import collection.immutable.List
import blueeyes.json.JPath
import java.lang.String
import blueeyes.json.JsonAST._

trait Mongo{
  def database(databaseName: String): MongoDatabase
}

trait MongoDatabase{
  def apply[T](query: MongoQuery[T]): T  = {
    val databaseCollection = query.collection match{
      case MongoCollectionReference(name)         => collection(name)
      case MongoCollectionHolder(realCollection)  => realCollection
    }
    query(databaseCollection)
  }

  def collection(collectionName: String): DatabaseCollection
}

trait DatabaseCollection{
  def insert(objects: List[JObject])
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]): Stream[JObject]
  def group(selection: MongoSelection, filter: Option[MongoFilter], initial: JObject, reduce: String): JArray
  def distinct(selection : JPath, filter: Option[MongoFilter]): List[JValue]
  def remove(filter: Option[MongoFilter])
  def count(filter: Option[MongoFilter]): Long
  def ensureIndex(name: String, keys: List[JPath], unique: Boolean)
  def dropIndexes
  def dropIndex(name: String)
  def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean)
  def mapReduce(map: String, reduce: String, outputCollection: Option[String], filter: Option[MongoFilter] = None): MapReduceOutput
}

trait QueryBehaviour[T] extends Function[DatabaseCollection, T]

trait EnsureIndexQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.ensureIndex(name, keys, unique)
    JNothing
  }
  def keys: List[JPath]
  def name: String
  def unique: Boolean
}

trait DropIndexQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.dropIndex(name)
    JNothing
  }
  def name: String
}
trait DropIndexesQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.dropIndexes
    JNothing
  }
}

trait InsertQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.insert(objects)
    JNothing
  }
  def objects: List[JObject]
}
trait MapReduceQueryBehaviour extends QueryBehaviour[MapReduceOutput]{
  def apply(collection: DatabaseCollection): MapReduceOutput = {
    collection.mapReduce(map, reduce, outputCollection, filter)
  }
  def map: String
  def reduce: String
  def collection: MongoCollection
  def filter: Option[MongoFilter]
  def outputCollection: Option[String]
}

trait RemoveQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection) = {
    collection.remove(filter)
    JNothing
  }

  def filter: Option[MongoFilter]
}
trait CountQueryBehaviour extends QueryBehaviour[JInt]{
  def apply(collection: DatabaseCollection): JInt = JInt(collection.count(filter))

  def filter: Option[MongoFilter]
}

trait SelectQueryBehaviour extends QueryBehaviour[Stream[JObject]]{
  def apply(collection: DatabaseCollection) = collection.select(selection, filter, sort, skip, limit)

  def selection : MongoSelection
  def filter    : Option[MongoFilter]
  def sort      : Option[MongoSort]
  def skip      : Option[Int]
  def limit     : Option[Int]
}
trait GroupQueryBehaviour extends QueryBehaviour[JArray]{
  def apply(collection: DatabaseCollection) = collection.group(selection, filter, initial, reduce)

  def selection : MongoSelection
  def reduce    : String
  def initial   : JObject
  def filter    : Option[MongoFilter]
}
trait DistinctQueryBehaviour extends QueryBehaviour[List[JValue]]{
  def apply(collection: DatabaseCollection) = collection.distinct(selection, filter)

  def selection : JPath
  def filter    : Option[MongoFilter]
}

trait SelectOneQueryBehaviour extends QueryBehaviour[Option[JObject]]{ self =>
  private val selectQuery = new SelectQueryBehaviour(){
    def limit     = Some(1)
    def skip      = None
    def sort      = self.sort
    def filter    = self.filter
    def selection = self.selection
  }
  def apply(collection: DatabaseCollection): Option[JObject] = selectQuery(collection).headOption
  
  def selection : MongoSelection
  def filter    : Option[MongoFilter]
  def sort      : Option[MongoSort]
}


trait UpdateQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection) = {
    value match {
      case MongoUpdateNothing =>
      case _ => collection.update(filter, value, upsert, multi)
    }
    JNothing
  }
  
  def value : MongoUpdateValue
  def filter: Option[MongoFilter]
  def upsert: Boolean
  def multi : Boolean
}
