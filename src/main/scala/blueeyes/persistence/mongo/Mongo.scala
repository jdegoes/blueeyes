package blueeyes.persistence.mongo

import collection.immutable.List
import blueeyes.persistence.mongo.json.MongoJson._
import scala.collection.JavaConversions._
import blueeyes.json.JPath
import java.lang.String
import blueeyes.json.JsonAST.{JInt, JField, JObject, JNothing}
import com.mongodb._

trait Mongo{
  def database(databaseName: String): MongoDatabase
}

trait DatabaseCollectionSource{
  def getCollection(collectionName: String): DatabaseCollection
}

trait DatabaseCollection{
  def insert(dbObjects: List[DBObject])
  def select(keys: DBObject, filter: DBObject, sort: Option[DBObject], skip: Option[Int], limit: Option[Int]): List[DBObject]
  def remove(filter: com.mongodb.DBObject): Int
  def ensureIndex(name: String, keys: DBObject, unique: Boolean)
}

class MongoDatabase(collectionSource: DatabaseCollectionSource){
  def apply[T](query: MongoQuery[T]): T = query(collectionSource.getCollection(query.collection.name))
}

trait QueryBehaviour[T] extends Function[DatabaseCollection, T]

trait EnsureIndexQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    val keysObject = JObject(keys.map(key => JField(key.toMongoField, JInt(1))))
    collection.ensureIndex(name, keysObject, unique)
    JNothing
  }
  def keys: List[JPath]
  def name: String
  def unique: Boolean
}

trait InsertQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.insert(objects.map(jObject2MongoObject(_)))
    JNothing
  }
  def objects: List[JObject]
}

trait RemoveQueryBehaviour extends QueryBehaviour[JInt]{
  def apply(collection: DatabaseCollection): JInt = JInt(collection.remove(jObject2MongoObject(filter.map(_.filter).getOrElse(JObject(Nil)))))

  def filter: Option[MongoFilter]
}

trait SelectQueryBehaviour extends QueryBehaviour[List[JObject]]{
  def apply(collection: DatabaseCollection): List[JObject] = {
    val keysObject   = JObject(selection.selection.map(key => JField(key.toMongoField, JInt(1))))
    val filterObject = filter.map(_.filter).getOrElse(JObject(Nil))
    val sortObject   = sort.map(v => JObject(JField(v.sortField.toMongoField, JInt(v.sortOrder.order)) :: Nil)).map(jObject2MongoObject(_))
    collection.select(keysObject, filterObject, sortObject, skip, limit).map(mongoObject2JObject(_))
  }
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


trait UpdateQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    JNothing
  }
  def value : JObject
  def filter: Option[MongoFilter]
  def upsert: Boolean
  def multi : Boolean
}

object RealMongo{
  class RealMongo(host: String, port: Int) extends Mongo{
    private lazy val mongo = new com.mongodb.Mongo(host , port)
    def database(databaseName: String) = new MongoDatabase(new RealDatabaseCollectionSource(mongo.getDB(databaseName)))
  }

  class RealDatabaseCollectionSource(database: DB) extends DatabaseCollectionSource{
    def getCollection(collectionName: String) = new RealDatabaseCollection(database.getCollection(collectionName))
  }

  class RealDatabaseCollection(collection: DBCollection) extends DatabaseCollection{
    def insert(dbObjects: List[DBObject]) = checkWriteResult(collection.insert(dbObjects))
    
    def remove(filter: DBObject)          = checkWriteResult(collection.remove(filter)).getN

    def ensureIndex(name: String, keys: DBObject, unique: Boolean) = collection.ensureIndex(keys, name, unique)


    def select(keys: DBObject, filter: DBObject, sort: Option[DBObject], skip: Option[Int], limit: Option[Int]): List[DBObject] = {
      val cursor        = collection.find(filter, keys)
      val sortedCursor  = sort.map(cursor.sort(_)).getOrElse(cursor)
      val skippedCursor = skip.map(sortedCursor.skip(_)).getOrElse(sortedCursor)
      val limitedCursor = limit.map(skippedCursor.limit(_)).getOrElse(skippedCursor)

      List(limitedCursor.toArray: _*)
    }

    private def checkWriteResult(result: WriteResult) = {
      val error  = result.getLastError
      if (error != null && error.get("err") != null){
       throw new MongoException(error)
      }
      result
    }    
  }
}

