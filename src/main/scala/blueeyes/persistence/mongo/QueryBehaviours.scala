package blueeyes.persistence.mongo

import scala.collection.IterableView
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import com.mongodb.MongoException
import blueeyes.concurrent._

private[mongo] object QueryBehaviours{
  trait QueryBehaviour[T] extends Function[DatabaseCollection, T]

  trait AsynchQueryBehaviour[T] extends QueryBehaviour[T]{
    def apply(collection: DatabaseCollection) = {
      collection.requestStart
      val result: Either[Throwable, T] = try {
        val answer    = query(collection)
        val lastError = collection.getLastError

        lastError.map(why => Left(new MongoException(why))).getOrElse(Right(answer))
      }
      catch {
       case error: Throwable => Left(error)
      }
      finally {
        collection.requestDone
      }

      result match {
        case Left(why)     => throw why
        case Right(answer) => answer
      }
    }

    def query(collection: DatabaseCollection): T
  }

  trait EnsureIndexQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.ensureIndex(name, keys, unique)
      JNothing
    }
    def keys: List[JPath]
    def name: String
    def unique: Boolean
  }

  trait DropIndexQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.dropIndex(name)
      JNothing
    }
    def name: String
  }
  trait DropIndexesQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.dropIndexes
      JNothing
    }
  }

  trait InsertQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.insert(objects)
      JNothing
    }
    def objects: List[JObject]
  }
  trait MapReduceQueryBehaviour extends AsynchQueryBehaviour[MapReduceOutput]{
    def query(collection: DatabaseCollection): MapReduceOutput = {
      collection.mapReduce(map, reduce, outputCollection, filter)
    }
    def map: String
    def reduce: String
    def collection: MongoCollection
    def filter: Option[MongoFilter]
    def outputCollection: Option[String]
  }

  trait RemoveQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection) = {
      collection.remove(filter)
      JNothing
    }

    def filter: Option[MongoFilter]
  }
  trait CountQueryBehaviour extends AsynchQueryBehaviour[JInt]{
    def query(collection: DatabaseCollection): JInt = JInt(collection.count(filter))

    def filter: Option[MongoFilter]
  }

  trait SelectQueryBehaviour extends AsynchQueryBehaviour[IterableView[JObject, Iterator[JObject]]]{
    def query(collection: DatabaseCollection) = {
      val i = 0
      collection.select(selection, filter, sort, skip, limit)
    }

    def selection : MongoSelection
    def filter    : Option[MongoFilter]
    def sort      : Option[MongoSort]
    def skip      : Option[Int]
    def limit     : Option[Int]
  }
  trait GroupQueryBehaviour extends AsynchQueryBehaviour[JArray]{
    def query(collection: DatabaseCollection) = collection.group(selection, filter, initial, reduce)

    def selection : MongoSelection
    def reduce    : String
    def initial   : JObject
    def filter    : Option[MongoFilter]
  }
  trait DistinctQueryBehaviour extends AsynchQueryBehaviour[List[JValue]]{
    def query(collection: DatabaseCollection) = collection.distinct(selection, filter)

    def selection : JPath
    def filter    : Option[MongoFilter]
  }

  trait SelectOneQueryBehaviour extends AsynchQueryBehaviour[Option[JObject]]{ self =>
    private val selectQuery = new SelectQueryBehaviour(){
      def limit     = Some(1)
      def skip      = None
      def sort      = self.sort
      def filter    = self.filter
      def selection = self.selection
    }
    def query(collection: DatabaseCollection): Option[JObject] = selectQuery.query(collection).headOption

    def selection : MongoSelection
    def filter    : Option[MongoFilter]
    def sort      : Option[MongoSort]
  }


  trait UpdateQueryBehaviour extends AsynchQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection) = {
      value match {
        case MongoUpdateNothing =>
        case _ => collection.update(filter, value, upsert, multi)
      }
      JNothing
    }

    def value : MongoUpdate
    def filter: Option[MongoFilter]
    def upsert: Boolean
    def multi : Boolean
  }
}