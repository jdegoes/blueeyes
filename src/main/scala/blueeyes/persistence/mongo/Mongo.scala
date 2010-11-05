package blueeyes.persistence.mongo

import collection.immutable.List
import blueeyes.json.JPath
import java.lang.String
import blueeyes.json.JsonAST.{JInt, JObject, JNothing}

trait Mongo{
  def database(databaseName: String): MongoDatabase
}

trait MongoDatabase{
  def apply[T](query: MongoQuery[T]): T
  def collection(collectionName: String): DatabaseCollection
}

trait DatabaseCollection{
  def insert(objects: List[JObject])
  def select(selection : MongoSelection, filter: Option[MongoFilter], sort: Option[MongoSort], skip: Option[Int], limit: Option[Int]): Stream[JObject]
  def remove(filter: Option[MongoFilter]): Int
  def ensureIndex(name: String, keys: List[JPath], unique: Boolean)
  def update(filter: Option[MongoFilter], value : MongoUpdateValue, upsert: Boolean, multi: Boolean): Int
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

trait InsertQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.insert(objects)
    JNothing
  }
  def objects: List[JObject]
}

trait RemoveQueryBehaviour extends QueryBehaviour[JInt]{
  def apply(collection: DatabaseCollection): JInt = JInt(collection.remove(filter))

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


trait UpdateQueryBehaviour extends QueryBehaviour[JInt]{
  def apply(collection: DatabaseCollection): JInt = JInt(collection.update(filter, value, upsert, multi))
  
  def value : MongoUpdateValue
  def filter: Option[MongoFilter]
  def upsert: Boolean
  def multi : Boolean
}
