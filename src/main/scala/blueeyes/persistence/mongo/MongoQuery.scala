package blueeyes.persistence.mongo

import blueeyes.json.JPath
import blueeyes.util.ProductPrefixUnmangler
import blueeyes.json.JsonAST._

case class MongoCollection(name: String)

sealed abstract class MongoSortOrder(val order: Int)
case object MongoSortOrderAscending extends MongoSortOrder(1)
case object MongoSortOrderDescending extends MongoSortOrder(-1)

case class MongoSort(sortField: JPath, sortOrder: MongoSortOrder){
  def >> : MongoSort = MongoSort(sortField, MongoSortOrderAscending)
  def << : MongoSort = MongoSort(sortField, MongoSortOrderDescending)
}

trait MongoImplicits {
  implicit def stringToMongoCollection(string: String): MongoCollection = MongoCollection(string)

  implicit def jpathToMongoSort(jpath: JPath): MongoSort = MongoSort(jpath, MongoSortOrderAscending)

  implicit def stringToMongoSort(string: String): MongoSort = MongoSort(JPath(string), MongoSortOrderAscending)

  implicit def stringToMongoUpdateBuilder(string: String): MongoUpdateBuilder = MongoUpdateBuilder(JPath(string))

  implicit def jpathToMongoUpdateBuilder(jpath: JPath): MongoUpdateBuilder = MongoUpdateBuilder(jpath)

  implicit def jvalueToMongoUpdateObject(value: JObject): MongoUpdateObject = MongoUpdateObject(value)

}

object MongoImplicits extends MongoImplicits

case class MongoSelection(selection: List[JPath])

sealed trait MongoQuery[T] extends QueryBehaviour[T]{
  def collection: MongoCollection;
}

case class MongoSelectQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                            sort: Option[MongoSort] = None, skip: Option[Int] = None, limit: Option[Int] = None) extends MongoQuery[Stream[JObject]] with SelectQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectQuery = MongoSelectQuery(selection, collection, Some(newFilter), sort, skip, limit)
  def sortBy(newSort: MongoSort)    : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, Some(newSort), skip, limit)
  def skip  (newSkip: Int)          : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, sort, Some(newSkip), limit)
  def limit (newLimit: Int)         : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, sort, skip, Some(newLimit))
}
case class MongoSelectOneQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                              sort: Option[MongoSort] = None) extends MongoQuery[Option[JObject]] with SelectOneQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectOneQuery = MongoSelectOneQuery(selection, collection, Some(newFilter), sort)
  def sortBy(newSort: MongoSort)    : MongoSelectOneQuery = MongoSelectOneQuery(selection, collection, filter, Some(newSort))
}
case class MongoRemoveQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[JInt] with RemoveQueryBehaviour{
  def where (newFilter: MongoFilter): MongoRemoveQuery = MongoRemoveQuery(collection, Some(newFilter))
}
case class MongoCountQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[JInt] with CountQueryBehaviour{
  def where (newFilter: MongoFilter): MongoCountQuery = MongoCountQuery(collection, Some(newFilter))
}
case class MongoInsertQuery(collection: MongoCollection, objects: List[JObject]) extends MongoQuery[JNothing.type] with InsertQueryBehaviour
case class MongoEnsureIndexQuery(collection: MongoCollection, name: String, keys: List[JPath], unique: Boolean) extends MongoQuery[JNothing.type] with EnsureIndexQueryBehaviour
case class MongoDropIndexQuery(collection: MongoCollection, name: String) extends MongoQuery[JNothing.type] with DropIndexQueryBehaviour
case class MongoDropIndexesQuery(collection: MongoCollection) extends MongoQuery[JNothing.type] with DropIndexesQueryBehaviour
case class MongoUpdateQuery(collection: MongoCollection, value: MongoUpdateValue, filter: Option[MongoFilter] = None, upsert: Boolean = false,
                            multi: Boolean = false) extends MongoQuery[JInt] with UpdateQueryBehaviour{
  def where  (newFilter: MongoFilter) : MongoUpdateQuery = MongoUpdateQuery(collection, value, Some(newFilter), upsert, multi)
}

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
  class SetQueryEntryPoint[T <: MongoQuery[_]](f: (MongoUpdateValue) => T){
    def set(value: MongoUpdateValue): T = f(value)
  }

  def select(selection: JPath*)                 = new FromQueryEntryPoint[MongoSelectQuery]   ((collection: MongoCollection) => {MongoSelectQuery(MongoSelection(List(selection: _*)), collection)})
  def selectOne(selection: JPath*)              = new FromQueryEntryPoint[MongoSelectOneQuery]((collection: MongoCollection) => {MongoSelectOneQuery(MongoSelection(List(selection: _*)), collection)})
  def remove                                    = new FromQueryEntryPoint[MongoRemoveQuery]   ((collection: MongoCollection) => {MongoRemoveQuery(collection)})
  def count                                     = new FromQueryEntryPoint[MongoCountQuery]    ((collection: MongoCollection) => {MongoCountQuery(collection)})
  def insert( value: JObject*)                  = new IntoQueryEntryPoint[MongoInsertQuery]   ((collection: MongoCollection) => {MongoInsertQuery(collection, List(value: _*))})
  def ensureIndex(name: String)                 = new OnKeysQueryEntryPoint[MongoEnsureIndexQuery]((collection: MongoCollection, keys: List[JPath]) => {MongoEnsureIndexQuery(collection, name, keys, false)})
  def ensureUniqueIndex(name: String)           = new OnKeysQueryEntryPoint[MongoEnsureIndexQuery]((collection: MongoCollection, keys: List[JPath]) => {MongoEnsureIndexQuery(collection, name, keys, true)})
  def dropIndex(name: String)                   = new OnQueryEntryPoint[MongoDropIndexQuery]((collection: MongoCollection) => {MongoDropIndexQuery(collection, name)})
  def dropIndexes                               = new OnQueryEntryPoint[MongoDropIndexesQuery]((collection: MongoCollection) => {MongoDropIndexesQuery(collection)})
  def update( collection: MongoCollection)      = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdateValue) => {MongoUpdateQuery(collection, value, None, false, false)})
  def updateMany( collection: MongoCollection)  = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdateValue) => {MongoUpdateQuery(collection, value, None, false, true)})
  def upsert( collection: MongoCollection)      = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdateValue) => {MongoUpdateQuery(collection, value, None, true, false)})
  def upsertMany( collection: MongoCollection)  = new SetQueryEntryPoint[MongoUpdateQuery]((value: MongoUpdateValue) => {MongoUpdateQuery(collection, value, None, true, true)})
}

object MongoUpdateOperators {
  sealed trait MongoUpdateOperator extends Product with ProductPrefixUnmangler {
    def symbol: String = unmangledName

    override def toString = symbol
  }

  case object $inc      extends MongoUpdateOperator
  case object $set      extends MongoUpdateOperator
  case object $unset    extends MongoUpdateOperator
  case object $push     extends MongoUpdateOperator
  case object $pushAll  extends MongoUpdateOperator
  case object $addToSet extends MongoUpdateOperator
  case object $pop      extends MongoUpdateOperator
  case object $pull     extends MongoUpdateOperator
  case object $pullAll  extends MongoUpdateOperator
}

import MongoUpdateOperators._
sealed trait MongoUpdateValue{
  def  toJValue: JObject;
}

sealed case class MongoUpdateObject(value: JObject) extends MongoUpdateValue{
  def toJValue = value
}

sealed case class MongoUpdateFieldValue(operator: MongoUpdateOperator, path: JPath, filter: MongoFilter) extends MongoUpdateValue{  self =>
  def toJValue: JObject = JObject(JField(operator.symbol, JObject(JField(JPathExtension.toMongoField(path), filter.filter) :: Nil)) :: Nil)

  def & (that: MongoUpdateFieldValue): MongoUpdateValue = MongoUpdateFieldsValues(self :: that :: Nil)
}

sealed case class MongoUpdateFieldsValues(values: List[MongoUpdateValue]) extends MongoUpdateValue{
  def toJValue: JObject = values.foldLeft(JObject(Nil)) { (obj, e) => obj.merge(e.toJValue).asInstanceOf[JObject] }
}

case class MongoUpdateBuilder(jpath: JPath) {
  import MongoFilterImplicits._
  import MongoFilterOperators._
  def inc [T](value: MongoPrimitive[T]) : MongoUpdateFieldValue = MongoUpdateFieldValue($inc,   jpath, "" === value)
  def set [T](value: MongoPrimitive[T]) : MongoUpdateFieldValue = MongoUpdateFieldValue($set,   jpath, "" === value)
  def unset                             : MongoUpdateFieldValue = MongoUpdateFieldValue($unset, jpath, "" === MongoPrimitiveInt(1))
  def popLast                           : MongoUpdateFieldValue = MongoUpdateFieldValue($pop,   jpath, "" === MongoPrimitiveInt(1))
  def popFirst                          : MongoUpdateFieldValue = MongoUpdateFieldValue($pop,   jpath, "" === MongoPrimitiveInt(-1))
  def push [T](value: MongoPrimitive[T]): MongoUpdateFieldValue = MongoUpdateFieldValue($push,  jpath, "" === value)
  def pull(filter: MongoFilter)         : MongoUpdateFieldValue = MongoUpdateFieldValue($pull,  jpath, filter)  
  def pushAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateFieldValue = MongoUpdateFieldValue($pushAll, jpath, "" === MongoPrimitiveArray(List(items: _*)))
  def pullAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateFieldValue = MongoUpdateFieldValue($pullAll, jpath, "" === MongoPrimitiveArray(List(items: _*)))

  def addToSet [T <: MongoPrimitive[_]](items: T*): MongoUpdateFieldValue = {
    val itemsList = List(items: _*)
    if (itemsList.size == 1) {
      val item: MongoPrimitive[_] = itemsList.head 
      MongoUpdateFieldValue($addToSet, jpath, "" === item)
    }
    else MongoUpdateFieldValue($addToSet, jpath, MongoFieldFilter(JPath(""), $each, MongoPrimitiveArray(itemsList)))
  }  
}