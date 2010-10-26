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
  def insert(dbObjects: List[com.mongodb.DBObject])
  def remove(filter: com.mongodb.DBObject)
  def ensureIndex(name: String, keys: com.mongodb.DBObject, unique: Boolean)
}

class MongoDatabase(collectionSource: DatabaseCollectionSource){
  def apply[T](query: MongoQuery[T]): T = {
    query(collectionSource.getCollection(query.collection.name))
  }
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


trait RemoveQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    collection.remove(jObject2MongoObject(filter.map(_.filter).getOrElse(JObject(Nil))))
    JNothing
  }
  def filter: Option[MongoFilter]
}

trait SelectQueryBehaviour extends QueryBehaviour[List[JObject]]{
  def apply(collection: DatabaseCollection): List[JObject] = {
    Nil
  }
  def selection : MongoSelection
  def filter    : Option[MongoFilter]
  def sort      : Option[MongoSort]
  def skip      : Option[Int]
  def limit     : Option[Int]
}

trait SelectOneQueryBehaviour extends QueryBehaviour[Option[JObject]]{
  def apply(collection: DatabaseCollection): Option[JObject] = {
    None
  }
  def selection : MongoSelection
  def filter    : Option[MongoFilter]
}


trait UpdateQueryBehaviour extends QueryBehaviour[JNothing.type]{
  def apply(collection: DatabaseCollection): JNothing.type = {
    JNothing
  }
  def value: JObject
  def filter: Option[MongoFilter]
  def upsert: Boolean
  def multi: Boolean
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
    
    def remove(filter: DBObject)        = checkWriteResult(collection.remove(filter))

    def ensureIndex(name: String, keys: DBObject, unique: Boolean) = collection.ensureIndex(keys, name, unique)

    private def checkWriteResult(result: WriteResult){
      val error  = result.getLastError
      if (error != null && error.get("err") != null){
       throw new MongoException(error)
      }
    }    
  }
}

