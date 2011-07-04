package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import blueeyes.concurrent._
import com.mongodb.MongoException
import scala.collection.IterableView
import scala.collection.immutable.ListSet
import scalaz.{Validation, Success, Failure}

private[mongo] object QueryBehaviours{
  trait QueryBehaviour[T] extends Function2[DatabaseCollection, Boolean, T]

  trait MongoQueryBehaviour[T] extends QueryBehaviour[T]{
    def apply(collection: DatabaseCollection, isVerified: Boolean = true) = {
      val result = try{
        if (isVerified) verifiedQuery(collection) else unverifiedQuery(collection)
      } catch {
        case error: Throwable => Failure(error)
      }

      result match {
        case Failure(why)     => throw why
        case Success(answer) => answer
      }
    }

    private def unverifiedQuery(collection: DatabaseCollection): Validation[Throwable, T] = Success(query(collection))

    private def verifiedQuery(collection: DatabaseCollection): Validation[Throwable, T] = try {
      collection.requestStart
      val answer    = query(collection)
      val lastError = collection.getLastError

      lastError.map(why => Failure(new MongoException(why))).getOrElse(Success(answer))
    } finally {
      collection.requestDone
    }

    def query(collection: DatabaseCollection): T
  }

  trait EnsureIndexQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.ensureIndex(name, keys, unique)
      JNothing
    }

    def keys: ListSet[JPath]
    def name: String
    def unique: Boolean
  }

  trait DropIndexQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.dropIndex(name)
      JNothing
    }

    def name: String
  }
  trait DropIndexesQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.dropIndexes
      JNothing
    }
  }

  trait InsertQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection): JNothing.type = {
      collection.insert(objects)
      JNothing
    }
    def objects: List[JObject]
  }
  trait MapReduceQueryBehaviour extends MongoQueryBehaviour[MapReduceOutput]{
    def query(collection: DatabaseCollection): MapReduceOutput = {
      collection.mapReduce(map, reduce, outputCollection, filter)
    }
    def map: String
    def reduce: String
    def collection: MongoCollection
    def filter: Option[MongoFilter]
    def outputCollection: Option[String]
  }

  trait RemoveQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
    def query(collection: DatabaseCollection) = {
      collection.remove(filter)
      JNothing
    }

    def filter: Option[MongoFilter]
  }
  trait CountQueryBehaviour extends MongoQueryBehaviour[JInt]{
    def query(collection: DatabaseCollection): JInt = JInt(collection.count(filter))

    def filter: Option[MongoFilter]
  }

  trait SelectQueryBehaviour extends MongoQueryBehaviour[IterableView[JObject, Iterator[JObject]]]{
    def query(collection: DatabaseCollection) = {
      collection.select(selection, filter, sort, skip, limit, hint, isSnapshot)
    }

    def selection  : MongoSelection
    def filter     : Option[MongoFilter]
    def sort       : Option[MongoSort]
    def skip       : Option[Int]
    def limit      : Option[Int]
    def hint       : Option[Hint]
    def isSnapshot : Boolean
  }
  trait ExplainQueryBehaviour  extends MongoQueryBehaviour[JObject]{
    def query(collection: DatabaseCollection) = {
      collection.explain(selection, filter, sort, skip, limit, hint, isSnapshot)
    }

    def selection : MongoSelection
    def filter    : Option[MongoFilter]
    def sort      : Option[MongoSort]
    def skip      : Option[Int]
    def limit     : Option[Int]
    def hint      : Option[Hint]
    def isSnapshot : Boolean
  }
  trait GroupQueryBehaviour extends MongoQueryBehaviour[JArray]{
    def query(collection: DatabaseCollection) = collection.group(selection, filter, initial, reduce)

    def selection : MongoSelection
    def reduce    : String
    def initial   : JObject
    def filter    : Option[MongoFilter]
  }
  trait DistinctQueryBehaviour extends MongoQueryBehaviour[List[JValue]]{
    def query(collection: DatabaseCollection) = collection.distinct(selection, filter)

    def selection : JPath
    def filter    : Option[MongoFilter]
  }

  trait SelectOneQueryBehaviour extends MongoQueryBehaviour[Option[JObject]]{ self =>
    private val selectQuery = new SelectQueryBehaviour() {
      def limit     = Some(1)
      def skip      = None
      def sort      = self.sort
      def filter    = self.filter
      def selection = self.selection
      def hint      = self.hint
      def isSnapshot = false
    }

    def query(collection: DatabaseCollection): Option[JObject] = {
      selectQuery.query(collection).headOption
    }

    def selection : MongoSelection
    def filter    : Option[MongoFilter]
    def sort      : Option[MongoSort]
    def hint      : Option[Hint]
  }
  trait MultiSelectQuery extends MongoQueryBehaviour[IterableView[Option[JObject], Seq[Option[JObject]]]]{ self =>
    import MongoFilterEvaluator._
    import IterableViewImpl._
    private val selectQuery = new SelectQueryBehaviour() {
      def limit     = None
      def skip      = None
      def sort      = self.sort
      def filter    = Some(MongoOrFilter(self.filters))
      def selection = MongoSelection(Set())
      def hint      = self.hint
      def isSnapshot = false
    }

    def query(collection: DatabaseCollection) = {
      val allObjects = selectQuery.query(collection)
      var result     = filters.toList.map(filter => (filter, None: Option[JObject]))

      allObjects.exists{jObject =>
        result = result.map{filterAndObject => filterAndObject match {
          case (filter, None) => (filter, List(jObject).filter(filter).headOption.asInstanceOf[Option[JObject]])
          case _ => filterAndObject
        }}
        result.find(_._2 == None).map(_ => false).getOrElse(true)
      }
      new IterableViewImpl[Option[JObject], Seq[Option[JObject]]](result.unzip._2.toSeq)
    }
    def filters   : ListSet[MongoFilter]
    def sort      : Option[MongoSort]
    def hint      : Option[Hint]
  }

  trait UpdateQueryBehaviour extends MongoQueryBehaviour[JNothing.type]{
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
