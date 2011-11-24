package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import blueeyes.concurrent._
import com.mongodb.MongoException
import scala.collection.IterableView
import scalaz.{Validation, Success, Failure}

private[mongo] object QueryBehaviours{
  trait QueryBehaviour {
    type QueryResult
    def apply(collection: DatabaseCollection, isVerified: Boolean): QueryResult 
  }

  trait MongoQueryBehaviour extends QueryBehaviour {
    /**
     * This should only be true for ops that can be verified via a call to getLastError
     */
    val isVerifiable : Boolean

    override def apply(collection: DatabaseCollection, isVerified: Boolean = true) = {
      val result = try{
        if (isVerified && isVerifiable) verifiedQuery(collection) else unverifiedQuery(collection)
      } catch {
        case error: Throwable => Failure(error)
      }

      result match {
        case Failure(why)     => throw why
        case Success(answer) => answer
      }
    }

    private def unverifiedQuery(collection: DatabaseCollection): Validation[Throwable, QueryResult] = Success(query(collection))

    private def verifiedQuery(collection: DatabaseCollection): Validation[Throwable, QueryResult] = try {
      collection.requestStart()
      val answer    = query(collection)
      val lastError = collection.getLastError

      lastError.map(why => Failure(new MongoException(why))).getOrElse(Success(answer))
    } finally {
      collection.requestDone()
    }

    def query(collection: DatabaseCollection): QueryResult
  }

  trait EnsureIndexQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Unit
    def query(collection: DatabaseCollection) { collection.ensureIndex(name, keys, unique, options) }

    def keys: Seq[(JPath, IndexType)]
    def name: String
    def unique: Boolean
    def options: JObject
  }

  trait DropIndexQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Unit
    def query(collection: DatabaseCollection) { collection.dropIndex(name) }

    def name: String
  }

  trait DropIndexesQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Unit
    def query(collection: DatabaseCollection) { collection.dropIndexes() }
  }

  trait InsertQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = true
    type QueryResult = Unit
    def query(collection: DatabaseCollection) { collection.insert(objects) }
    def objects: List[JObject]
  }

  trait MapReduceQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = MapReduceOutput
    def query(collection: DatabaseCollection): MapReduceOutput = {
      collection.mapReduce(map, reduce, outputCollection, filter)
    }
    def map: String
    def reduce: String
    def collection: MongoCollection
    def filter: Option[MongoFilter]
    def outputCollection: Option[String]
  }

  trait RemoveQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = true
    type QueryResult = Unit
    def query(collection: DatabaseCollection) { collection.remove(filter) }

    def filter: Option[MongoFilter]
  }

  trait CountQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Long
    def query(collection: DatabaseCollection) = collection.count(filter)

    def filter: Option[MongoFilter]
  }

  trait SelectQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = IterableView[JObject, Iterator[JObject]]
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

  trait ExplainQueryBehaviour  extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = JObject
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

  trait GroupQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = JArray
    def query(collection: DatabaseCollection) = collection.group(selection, filter, initial, reduce)

    def selection : MongoSelection
    def reduce    : String
    def initial   : JObject
    def filter    : Option[MongoFilter]
  }

  trait DistinctQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = List[JValue]
    def query(collection: DatabaseCollection) = collection.distinct(selection, filter)

    def selection : JPath
    def filter    : Option[MongoFilter]
  }

  trait SelectOneQueryBehaviour extends MongoQueryBehaviour { self =>
    val isVerifiable = false
    type QueryResult = Option[JObject]
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

  trait MultiSelectQuery extends MongoQueryBehaviour { self =>
    val isVerifiable = false
    type QueryResult = IterableView[Option[JObject], Seq[Option[JObject]]]
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
      var result     = filters.map(filter => (filter, None: Option[JObject]))

      allObjects.exists{jObject =>
        result = result.map{filterAndObject => filterAndObject match {
          case (filter, None) => (filter, List(jObject).filter(filter).headOption.asInstanceOf[Option[JObject]])
          case _ => filterAndObject
        }}
        result.find(_._2 == None).map(_ => false).getOrElse(true)
      }
      new IterableViewImpl[Option[JObject], Seq[Option[JObject]]](result.unzip._2.toSeq)
    }
    def filters   : Seq[MongoFilter]
    def sort      : Option[MongoSort]
    def hint      : Option[Hint]
  }

  trait UpdateQueryBehaviour extends MongoQueryBehaviour {
    val isVerifiable = true
    type QueryResult = Unit
    def query(collection: DatabaseCollection) = value match {
      case MongoUpdateNothing =>
      case _ => collection.update(filter, value, upsert, multi)
    }

    def value : MongoUpdate
    def filter: Option[MongoFilter]
    def upsert: Boolean
    def multi : Boolean
  }

  trait SelectAndUpdateQueryBehaviour  extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Option[JObject]
    def query(collection: DatabaseCollection) = update match {
      case MongoUpdateNothing => throw new MongoException("""findAndModifyFailed failed: "exception: must specify remove or update"""")
      case _ => collection.selectAndUpdate(filter, sort, update, selection, returnNew, upsert)
    }

    def update: MongoUpdate
    def filter: Option[MongoFilter]
    def sort: Option[MongoSort]
    def selection: MongoSelection
    def returnNew: Boolean
    def upsert: Boolean
  }

  trait SelectAndRemoveQueryBehaviour  extends MongoQueryBehaviour {
    val isVerifiable = false
    type QueryResult = Option[JObject]
    def query(collection: DatabaseCollection) = collection.selectAndRemove(filter, sort, selection)

    def filter: Option[MongoFilter]
    def sort: Option[MongoSort]
    def selection: MongoSelection
  }
}
