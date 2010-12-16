package blueeyes.persistence.mongo

import blueeyes.json.JsonAST._
import blueeyes.json.{JPathImplicits, JPath}
import QueryBehaviours._

sealed trait MongoCollection
case class MongoCollectionReference(name: String) extends MongoCollection
case class MongoCollectionHolder(collection: DatabaseCollection) extends MongoCollection  

sealed abstract class MongoSortOrder(val order: Int)
case object MongoSortOrderAscending extends MongoSortOrder(1)
case object MongoSortOrderDescending extends MongoSortOrder(-1)

/** The MongoSort defines sort order of mongo search result. 
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val sortOrder = "foo.bar" <<
 *
 * val query  = selectOne().from("mycollection").sortBy(sortOrder)
 * val query2 = selectOne().from("mycollection").sortBy("foo.bar" <<)
 * </pre>
 */
case class MongoSort(sortField: JPath, sortOrder: MongoSortOrder){
  def >> : MongoSort = MongoSort(sortField, MongoSortOrderAscending)
  def << : MongoSort = MongoSort(sortField, MongoSortOrderDescending)
}

trait MongoImplicits extends JPathImplicits with MongoFilterImplicits {
  implicit def stringToMongoCollection(string: String): MongoCollection = MongoCollectionReference(string)

  implicit def jpathToMongoSort(jpath: JPath): MongoSort = MongoSort(jpath, MongoSortOrderAscending)

  implicit def stringToMongoSort(string: String): MongoSort = MongoSort(JPath(string), MongoSortOrderAscending)

  implicit def stringToMongoUpdateBuilder(string: String): MongoUpdateBuilder = MongoUpdateBuilder(JPath(string))

  implicit def jpathToMongoUpdateBuilder(jpath: JPath): MongoUpdateBuilder = MongoUpdateBuilder(jpath)

  implicit def jvalueToMongoUpdateObject(value: JObject): MongoUpdateObject = MongoUpdateObject(value)
}

object MongoImplicits extends MongoImplicits

case class MongoSelection(selection: List[JPath])

/** Basic trait for mongo queries.
 */
sealed trait MongoQuery[T] extends QueryBehaviour[T]{
  def collection: MongoCollection;
}

case class MongoDistinctQuery(selection: JPath, collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[List[JValue]] with DistinctQueryBehaviour{
  def where (newFilter: MongoFilter): MongoDistinctQuery = copy(filter = Some(newFilter))
}
case class MongoGroupQuery(selection: MongoSelection, collection: MongoCollection, initial: JObject, reduce: String, filter: Option[MongoFilter] = None) extends MongoQuery[JArray] with GroupQueryBehaviour{
  def where (newFilter: MongoFilter): MongoGroupQuery = copy(filter = Some(newFilter))
}
case class MongoSelectQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                            sort: Option[MongoSort] = None, skip: Option[Int] = None, limit: Option[Int] = None) extends MongoQuery[Stream[JObject]] with SelectQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectQuery = copy(filter = Some(newFilter))
  def sortBy(newSort: MongoSort)    : MongoSelectQuery = copy(sort = Some(newSort))
  def skip  (newSkip: Int)          : MongoSelectQuery = copy(skip = Some(newSkip))
  def limit (newLimit: Int)         : MongoSelectQuery = copy(limit = Some(newLimit))
}
case class MongoSelectOneQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                              sort: Option[MongoSort] = None) extends MongoQuery[Option[JObject]] with SelectOneQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectOneQuery = copy(filter = Some(newFilter))
  def sortBy(newSort: MongoSort)    : MongoSelectOneQuery = copy(sort = Some(newSort))
}
case class MongoRemoveQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[JNothing.type] with RemoveQueryBehaviour{
  def where (newFilter: MongoFilter): MongoRemoveQuery = copy(filter = Some(newFilter))
}
case class MongoCountQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[JInt] with CountQueryBehaviour{
  def where (newFilter: MongoFilter): MongoCountQuery = copy(filter = Some(newFilter))
}
case class MongoInsertQuery(collection: MongoCollection, objects: List[JObject]) extends MongoQuery[JNothing.type] with InsertQueryBehaviour
case class MongoEnsureIndexQuery(collection: MongoCollection, name: String, keys: List[JPath], unique: Boolean) extends MongoQuery[JNothing.type] with EnsureIndexQueryBehaviour
case class MongoDropIndexQuery(collection: MongoCollection, name: String) extends MongoQuery[JNothing.type] with DropIndexQueryBehaviour
case class MongoDropIndexesQuery(collection: MongoCollection) extends MongoQuery[JNothing.type] with DropIndexesQueryBehaviour
case class MongoUpdateQuery(collection: MongoCollection, value: MongoUpdate, filter: Option[MongoFilter] = None, upsert: Boolean = false,
                            multi: Boolean = false) extends MongoQuery[JNothing.type] with UpdateQueryBehaviour{
  def where  (newFilter: MongoFilter) : MongoUpdateQuery = copy(filter = Some(newFilter))
}
case class MongoMapReduceQuery(map: String, reduce: String, collection: MongoCollection, outputCollection: Option[String] = None, filter: Option[MongoFilter] = None)
                            extends MongoQuery[MapReduceOutput] with MapReduceQueryBehaviour{
  def where (newFilter: MongoFilter)    = copy(filter = Some(newFilter))
  def into(newOutputCollection: String) = copy(outputCollection = Some(newOutputCollection))
}


trait MapReduceOutput{
  def outpotCollection: MongoCollection
  def drop: Unit
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

object MongoQueryBuilder{
  class FromQueryEntryPoint[T <: MongoQuery[_]](f: (MongoCollection) => T){
    def from (collection: MongoCollection): T = f(collection)
  }
  class IntoQueryEntryPoint[T <: MongoQuery[_]](f: (MongoCollection) => T){
    def into (collection: MongoCollection): T = f(collection)
  }
  class OnKeysQueryEntryPoint[T <: MongoQuery[_]](f: (MongoCollection, List[JPath]) => T){
    def on(collection: MongoCollection, keys: JPath*): T = f(collection, List(keys: _*))
  }
  class OnQueryEntryPoint[T <: MongoQuery[_]](f: (MongoCollection) => T){
    def on(collection: MongoCollection): T = f(collection)
  }
  class SetQueryEntryPoint[T <: MongoQuery[_]](f: (MongoUpdate) => T){
    def set(value: MongoUpdate): T = f(value)
  }

  def select(selection: JPath*)                 = new FromQueryEntryPoint[MongoSelectQuery]   ((collection: MongoCollection) => {MongoSelectQuery(MongoSelection(List(selection: _*)), collection)})
  def distinct(selection: JPath)                = new FromQueryEntryPoint[MongoDistinctQuery] ((collection: MongoCollection) => {MongoDistinctQuery(selection, collection)})
  def selectOne(selection: JPath*)              = new FromQueryEntryPoint[MongoSelectOneQuery]((collection: MongoCollection) => {MongoSelectOneQuery(MongoSelection(List(selection: _*)), collection)})
  def remove                                    = new FromQueryEntryPoint[MongoRemoveQuery]   ((collection: MongoCollection) => {MongoRemoveQuery(collection)})
  def count                                     = new FromQueryEntryPoint[MongoCountQuery]    ((collection: MongoCollection) => {MongoCountQuery(collection)})
  def insert( value: JObject*)                  = new IntoQueryEntryPoint[MongoInsertQuery]   ((collection: MongoCollection) => {MongoInsertQuery(collection, List(value: _*))})
  def ensureIndex(name: String)                 = new OnKeysQueryEntryPoint[MongoEnsureIndexQuery]((collection: MongoCollection, keys: List[JPath]) => {MongoEnsureIndexQuery(collection, name, keys, false)})
  def ensureUniqueIndex(name: String)           = new OnKeysQueryEntryPoint[MongoEnsureIndexQuery]((collection: MongoCollection, keys: List[JPath]) => {MongoEnsureIndexQuery(collection, name, keys, true)})
  def dropIndex(name: String)                   = new OnQueryEntryPoint[MongoDropIndexQuery]((collection: MongoCollection) => {MongoDropIndexQuery(collection, name)})
  def dropIndexes                               = new OnQueryEntryPoint[MongoDropIndexesQuery]((collection: MongoCollection) => {MongoDropIndexesQuery(collection)})
  def update( collection: MongoCollection)      = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdate) => {MongoUpdateQuery(collection, value, None, false, false)})
  def updateMany( collection: MongoCollection)  = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdate) => {MongoUpdateQuery(collection, value, None, false, true)})
  def upsert( collection: MongoCollection)      = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdate) => {MongoUpdateQuery(collection, value, None, true, false)})
  def upsertMany( collection: MongoCollection)  = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdate) => {MongoUpdateQuery(collection, value, None, true, true)})
  def mapReduce(map: String, reduce: String)    = new FromQueryEntryPoint[MongoMapReduceQuery](((collection: MongoCollection) => {MongoMapReduceQuery(map, reduce, collection, None, None)}))
  def group(initial: JObject, reduce: String, selection: JPath*) = new FromQueryEntryPoint[MongoGroupQuery]   ((collection: MongoCollection) => {MongoGroupQuery(MongoSelection(List(selection: _*)), collection, initial, reduce)})
}