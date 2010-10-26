package blueeyes.persistence.mongo

import blueeyes.json.JPath
import blueeyes.util.ProductPrefixUnmangler
import blueeyes.json.JsonAST.{JValue, JField, JObject, JNothing}

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

  implicit def mongoUpdateValueToJValue(value: MongoUpdateValue): JObject = value.toJValue
}

object MongoImplicits extends MongoImplicits

case class MongoSelection(selection: List[JPath])

sealed trait MongoQuery[T] extends QueryBehaviour[T]{
  def collection: MongoCollection;
}

case class MongoSelectQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None,
                            sort: Option[MongoSort] = None, skip: Option[Int] = None, limit: Option[Int] = None) extends MongoQuery[List[JObject]] with SelectQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectQuery = MongoSelectQuery(selection, collection, Some(newFilter), sort, skip, limit)
  def sortBy(newSort: MongoSort)    : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, Some(newSort), skip, limit)
  def skip  (newSkip: Int)          : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, sort, Some(newSkip), limit)
  def limit (newLimit: Int)         : MongoSelectQuery = MongoSelectQuery(selection, collection, filter, sort, skip, Some(newLimit))
}
case class MongoSelectOneQuery(selection: MongoSelection, collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[Option[JObject]] with SelectOneQueryBehaviour{
  def where (newFilter: MongoFilter): MongoSelectOneQuery = MongoSelectOneQuery(selection, collection, Some(newFilter))
}
case class MongoRemoveQuery(collection: MongoCollection, filter: Option[MongoFilter] = None) extends MongoQuery[JNothing.type] with RemoveQueryBehaviour{
  def where (newFilter: MongoFilter): MongoRemoveQuery = MongoRemoveQuery(collection, Some(newFilter))
}
case class MongoInsertQuery(collection: MongoCollection, objects: List[JObject]) extends MongoQuery[JNothing.type] with InsertQueryBehaviour
case class MongoUpdateQuery(collection: MongoCollection, value: JObject, filter: Option[MongoFilter] = None, upsert: Boolean = false,
                            multi: Boolean = false) extends MongoQuery[JNothing.type] with UpdateQueryBehaviour{
  def where  (newFilter: MongoFilter) : MongoUpdateQuery = MongoUpdateQuery(collection, value, Some(newFilter), upsert, multi)
}

object MongoQueryBuilder{

  sealed trait MongoQueryEntryPoint
  case class MongoSelectQueryEntryPoint(selection: MongoSelection) extends MongoQueryEntryPoint{
    def from(collection: MongoCollection) = MongoSelectQuery(selection, collection)
  }
  case class MongoSelectOneQueryEntryPoint(selection: MongoSelection) extends MongoQueryEntryPoint{
    def from(collection: MongoCollection) = MongoSelectOneQuery(selection, collection)
  }
  case class MongoRemoveQueryEntryPoint() extends MongoQueryEntryPoint{
    def from(collection: MongoCollection) = MongoRemoveQuery(collection)
  }  
  case class MongoInsertQueryEntryPoint(value: List[JObject]) extends MongoQueryEntryPoint{
    def into(collection: MongoCollection) = MongoInsertQuery(collection, value)
  }
  case class MongoUpdateQueryEntryPoint(collection: MongoCollection, upsert: Boolean = false, multi: Boolean = false) extends MongoQueryEntryPoint{
    def set(value: JObject) = MongoUpdateQuery(collection, value, None, upsert, multi)
  }

  def select(selection: JPath*)     = MongoSelectQueryEntryPoint(MongoSelection(List(selection: _*)))
  def selectOne(selection: JPath*)  = MongoSelectOneQueryEntryPoint(MongoSelection(List(selection: _*)))
  def remove                        = MongoRemoveQueryEntryPoint()
  def insert( value: JObject*)      = MongoInsertQueryEntryPoint(List(value: _*))
  def update( collection: MongoCollection)      = MongoUpdateQueryEntryPoint(collection)
  def updateMany( collection: MongoCollection)  = MongoUpdateQueryEntryPoint(collection, false, true)  
  def upsert( collection: MongoCollection)      = MongoUpdateQueryEntryPoint(collection, true, false)
  def upsertMany( collection: MongoCollection)  = MongoUpdateQueryEntryPoint(collection, true, true)
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
sealed trait MongoUpdateValue{ self =>
  def  toJValue: JObject;
  def & (that: MongoUpdateValue): MongoUpdateValue = MongoUpdateFieldsValues(self :: that :: Nil)
}

sealed case class MongoUpdateFieldValue(lhs: JPath, operator: MongoUpdateOperator, value: JValue) extends MongoUpdateValue{
  def toJValue: JObject = JObject(JField(operator.symbol, JObject(JField(lhs.toMongoField, value) :: Nil)) :: Nil)
}

sealed case class MongoUpdateFieldsValues(criteria: List[MongoUpdateValue]) extends MongoUpdateValue{
  def toJValue: JObject = criteria.foldLeft(JObject(Nil)) { (obj, e) => obj.merge(e.toJValue).asInstanceOf[JObject] }
}

case class MongoUpdateBuilder(jpath: JPath) {
  import MongoFilterImplicits._
  import MongoFilterOperators._
  def inc [T](value: MongoPrimitive[T]) : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $inc, value.toJValue)
  def set [T](value: MongoPrimitive[T]) : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $set, value.toJValue)
  def unset                             : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $unset, MongoPrimitiveInt(1).toJValue)
  def popLast                           : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $pop, MongoPrimitiveInt(1).toJValue)
  def popFirst                          : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $pop,MongoPrimitiveInt(-1).toJValue)
  def push [T](value: MongoPrimitive[T]): MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $push, value.toJValue)
  def pull [T](value: MongoPrimitive[T], operator: MongoFilterOperator = $eq): MongoUpdateFieldValue = {
    val jValue = operator match {
      case $eq => value.toJValue
      case _ => JObject(JField(operator.symbol, value.toJValue) :: Nil)
    }
    MongoUpdateFieldValue(jpath, $pull, jValue)
  }
  def pull (matchingCriteria: JObject)  : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $pull, matchingCriteria)

  def pushAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $pushAll, MongoPrimitiveArray(List(items: _*)).toJValue)
  def pullAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateFieldValue = MongoUpdateFieldValue(jpath, $pullAll, MongoPrimitiveArray(List(items: _*)).toJValue)
  def addToSet [T <: MongoPrimitive[_]](items: T*): MongoUpdateFieldValue = {
    val itemsList = List(items: _*)
    val value     = if (itemsList.size == 1) itemsList.head.toJValue
                    else JObject(JField("$each", MongoPrimitiveArray(itemsList).toJValue) :: Nil)
    MongoUpdateFieldValue(jpath, $addToSet, value)
  }  
}