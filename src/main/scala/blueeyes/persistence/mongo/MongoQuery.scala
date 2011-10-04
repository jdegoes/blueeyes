package blueeyes.persistence.mongo

import scala.collection.IterableView
import blueeyes.json.JsonAST._
import blueeyes.json.JPath
import QueryBehaviours._


sealed trait MongoCollection{
  def name: String
}
case class MongoCollectionReference(name: String) extends MongoCollection
case class MongoCollectionHolder(private[mongo] collection: DatabaseCollection, name: String, database: Database) extends MongoCollection

case class MongoDatabase(name: String)

sealed abstract class MongoSortOrder(val order: Int)
case object MongoSortOrderAscending extends MongoSortOrder(1)
case object MongoSortOrderDescending extends MongoSortOrder(-1)

/** The MongoSort defines sort order of mongo search result. 
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val sortOrder = "foo.bar" &lt;&lt;
 *
 * val query  = selectOne().from("mycollection").sortBy(sortOrder)
 * val query2 = selectOne().from("mycollection").sortBy("foo.bar" &lt;&lt;)
 * </pre>
 */
case class MongoSort(sortField: JPath, sortOrder: MongoSortOrder){
  def >> : MongoSort = MongoSort(sortField, MongoSortOrderAscending)
  def << : MongoSort = MongoSort(sortField, MongoSortOrderDescending)
}

case class MongoSelection(selection: Set[JPath])

sealed trait Hint
case class NamedHint(indexName: String) extends Hint
case class KeyedHint(keys: Seq[JPath]) extends Hint

sealed trait IndexType
case object OrdinaryIndex extends IndexType
case object GeospatialIndex extends IndexType

/** Basic trait for mongo queries.
 */
sealed trait MongoQuery extends QueryBehaviour {
  def collection: MongoCollection;
}

case class MongoExplainQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                            sort: Option[MongoSort] = None, skip: Option[Int] = None, limit: Option[Int] = None, hint: Option[Hint] = None, isSnapshot: Boolean = false) extends MongoQuery with ExplainQueryBehaviour

case class MongoDistinctQuery(selection: JPath, collection: MongoCollection, filter: Option[MongoFilter] = None, hint: Option[Hint] = None) extends MongoQuery with DistinctQueryBehaviour{
  def where (newFilter: MongoFilter): MongoDistinctQuery = copy(filter = Some(newFilter))
  def hint  (newHint: Hint)         : MongoDistinctQuery = copy(hint = Some(newHint))
}
case class MongoGroupQuery(selection: MongoSelection, collection: MongoCollection, initial: JObject, reduce: String, filter: Option[MongoFilter] = None) extends MongoQuery with GroupQueryBehaviour{
  def where (newFilter: MongoFilter): MongoGroupQuery = copy(filter = Some(newFilter))
}
case class MongoSelectQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                            sort: Option[MongoSort] = None, skip: Option[Int] = None, limit: Option[Int] = None, hint: Option[Hint] = None, isSnapshot: Boolean = false) extends MongoQuery with SelectQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectQuery = copy(filter = Some(newFilter))
  def sortBy(newSort: MongoSort)    : MongoSelectQuery = copy(sort = Some(newSort))
  def skip  (newSkip: Int)          : MongoSelectQuery = copy(skip = Some(newSkip))
  def limit (newLimit: Int)         : MongoSelectQuery = copy(limit = Some(newLimit))
  def hint  (newHint: Hint)         : MongoSelectQuery = copy(hint = Some(newHint))
  def snapshot                      : MongoSelectQuery = copy(isSnapshot = true)
  def explain                       : MongoExplainQuery = MongoExplainQuery(selection, collection, filter, sort, skip, limit, hint, isSnapshot)
}
case class MongoMultiSelectQuery(filters: Seq[MongoFilter], collection: MongoCollection,
                                sort: Option[MongoSort] = None, hint: Option[Hint] = None) extends MongoQuery with MultiSelectQuery{
  require(!filters.isEmpty)
  def hint(newHint: Hint)           : MongoMultiSelectQuery = copy(hint = Some(newHint))
  def sortBy(newSort: MongoSort)    : MongoMultiSelectQuery = copy(sort = Some(newSort))
  def explain                       : MongoExplainQuery = MongoExplainQuery(MongoSelection(Set()), collection, Some(MongoOrFilter(filters)), sort, None, None, hint, false)
}
case class MongoSelectOneQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                              sort: Option[MongoSort] = None, hint: Option[Hint] = None) extends MongoQuery with SelectOneQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectOneQuery = copy(filter = Some(newFilter))
  def sortBy(newSort: MongoSort)    : MongoSelectOneQuery = copy(sort = Some(newSort))
  def hint(newHint: Hint)           : MongoSelectOneQuery = copy(hint = Some(newHint))
  def explain                       : MongoExplainQuery = MongoExplainQuery(selection, collection, filter, sort, None, None, hint, false)
}
case class MongoRemoveQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery with RemoveQueryBehaviour{
  def where (newFilter: MongoFilter): MongoRemoveQuery = copy(filter = Some(newFilter))
}
case class MongoCountQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery with CountQueryBehaviour{
  def where (newFilter: MongoFilter): MongoCountQuery = copy(filter = Some(newFilter))
}
case class MongoInsertQuery(collection: MongoCollection, objects: List[JObject]) extends MongoQuery with InsertQueryBehaviour
case class MongoEnsureIndexQuery(collection: MongoCollection, name: String, keys: Seq[(JPath, IndexType)], unique: Boolean, options: JObject = JObject(Nil)) extends MongoQuery with EnsureIndexQueryBehaviour{
  def geospatial(path: JPath) = copy(keys = keys.map(key => if (key._1 == path) (path, GeospatialIndex) else key))
  def options(newOptions: JObject) = copy(options = newOptions)
}
case class MongoDropIndexQuery(collection: MongoCollection, name: String) extends MongoQuery with DropIndexQueryBehaviour
case class MongoDropIndexesQuery(collection: MongoCollection) extends MongoQuery with DropIndexesQueryBehaviour
case class MongoUpdateQuery(collection: MongoCollection, value: MongoUpdate, filter: Option[MongoFilter] = None, upsert: Boolean = false,
                            multi: Boolean = false) extends MongoQuery with UpdateQueryBehaviour{
  def where  (newFilter: MongoFilter) : MongoUpdateQuery = copy(filter = Some(newFilter))
}
case class MongoMapReduceQuery(map: String, reduce: String, collection: MongoCollection, outputCollection: Option[String] = None, filter: Option[MongoFilter] = None)
                            extends MongoQuery with MapReduceQueryBehaviour{
  def where (newFilter: MongoFilter)    = copy(filter = Some(newFilter))
  def into(newOutputCollection: String) = copy(outputCollection = Some(newOutputCollection))
}

trait MapReduceOutput {
  def outputCollection: MongoCollection
  def drop(): Unit
}

/** The MongoQueryBuilder creates mongo queries.
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val query = selectOne().from("mycollection")
 * </pre>
 */

trait MongoQueryBuilder{
  class FromQueryEntryPoint[T <: MongoQuery](f: MongoCollection => T){
    def from (collection: MongoCollection): T = f(collection)
  }
  class IntoQueryEntryPoint[T <: MongoQuery](f: MongoCollection => T){
    def into (collection: MongoCollection): T = f(collection)
  }
  class OnKeysQueryEntryPoint[T](f: Seq[JPath] => T){
    def on(keys: JPath*): T = f(keys)
  }
  class InQueryEntryPoint[T <: MongoQuery](f: MongoCollection => T){
    def in(collection: MongoCollection): T = f(collection)
  }
  class SetQueryEntryPoint[T <: MongoQuery](f: MongoUpdate => T){
    def set(value: MongoUpdate): T = f(value)
  }

  def multiSelect(filters: MongoFilter*)        = new FromQueryEntryPoint(MongoMultiSelectQuery(filters, _))
  def select(selection: JPath*)                 = new FromQueryEntryPoint(MongoSelectQuery(MongoSelection(Set(selection: _*)), _))
  def selectAll                                 = select()
  def distinct(selection: JPath)                = new FromQueryEntryPoint(MongoDistinctQuery(selection, _))
  def selectOne(selection: JPath*)              = new FromQueryEntryPoint(MongoSelectOneQuery(MongoSelection(Set(selection: _*)), _))
  def remove                                    = new FromQueryEntryPoint(MongoRemoveQuery(_))
  def count                                     = new FromQueryEntryPoint(MongoCountQuery(_))
  def insert(value: JObject*)                   = new IntoQueryEntryPoint(MongoInsertQuery(_, List(value: _*)))
  def ensureIndex(name: String)                 = new OnKeysQueryEntryPoint(keys => new InQueryEntryPoint(MongoEnsureIndexQuery(_, name, keys.map((_, OrdinaryIndex)), false)))
  def ensureUniqueIndex(name: String)           = new OnKeysQueryEntryPoint(keys => new InQueryEntryPoint(MongoEnsureIndexQuery(_, name, keys.map((_, OrdinaryIndex)), true)))

  def dropIndex(name: String)                   = new InQueryEntryPoint(MongoDropIndexQuery(_, name))
  def dropIndexes                               = new InQueryEntryPoint(MongoDropIndexesQuery(_))

  def update(collection: MongoCollection)       = new SetQueryEntryPoint(MongoUpdateQuery(collection, _, None, false, false))
  def updateMany(collection: MongoCollection)   = new SetQueryEntryPoint(MongoUpdateQuery(collection, _, None, false, true))
  def upsert(collection: MongoCollection)       = new SetQueryEntryPoint(MongoUpdateQuery(collection, _, None, true, false))
  def upsertMany(collection: MongoCollection)   = new SetQueryEntryPoint(MongoUpdateQuery(collection, _, None, true, true))

  def mapReduce(map: String, reduce: String)    = new FromQueryEntryPoint(MongoMapReduceQuery(map, reduce, _, None, None))
  def group(initial: JObject, reduce: String, selection: JPath*) = new FromQueryEntryPoint(MongoGroupQuery(MongoSelection(Set(selection: _*)), _, initial, reduce))
}

object MongoQueryBuilder extends MongoQueryBuilder
