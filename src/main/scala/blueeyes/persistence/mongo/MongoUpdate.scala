package blueeyes.persistence.mongo

import blueeyes.util.ProductPrefixUnmangler
import blueeyes.json.JsonAST.{JField, JObject}
import blueeyes.json.{JPath}
import MongoImplicits._
import MongoFilterOperators._

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
case class MongoUpdateBuilder(jpath: JPath) {
  def inc [T](value: MongoPrimitive[T]) : MongoUpdateField = IncF(jpath, "" === value)
  def set [T](value: MongoPrimitive[T]) : MongoUpdateField = SetF(jpath, "" === value)
  def unset                             : MongoUpdateField = UnsetF(jpath)
  def popLast                           : MongoUpdateField = PopLastF(jpath)
  def popFirst                          : MongoUpdateField = PopFirstF(jpath)
  def push [T](value: MongoPrimitive[T]): MongoUpdateField = PushF(jpath, "" === value)
  def pull(filter: MongoFilter)         : MongoUpdateField = PullF(jpath, filter)
  def pushAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateField = PushAllF(jpath, "" === MongoPrimitiveArray(List(items: _*)))
  def pullAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateField = PullAllF(jpath, "" === MongoPrimitiveArray(List(items: _*)))

  def addToSet [T <: MongoPrimitive[_]](items: T*): MongoUpdateField = {
    val itemsList = List(items: _*)
    if (itemsList.size == 1) {
      val item: MongoPrimitive[_] = itemsList.head
      AddToSetF(jpath, "" === item)
    }
    else AddToSetF(jpath, MongoFieldFilter(JPath(""), $each, MongoPrimitiveArray(itemsList)))
  }
}

import Changes._
sealed trait MongoUpdate{ self =>

  import Changelist._
  import MongoUpdateObject._

  private implicit def toMongoUpdate(changes: List[Change1])         = MongoUpdateFields(changes.map(_.asInstanceOf[MongoUpdateField]))
  private implicit def toChanges(update: MongoUpdateFields)    = update.list
  private implicit def toChangeList(update: MongoUpdateFields) = Changelist(update.list)
  private def toUpdateFieldsValues(update: MongoUpdateObject)        = MongoUpdateFields(decompose(update.value))

  def  toJValue: JObject;

  def & (that: MongoUpdate): MongoUpdate = (self, that) match {
    case (x: MongoUpdateField,   y: MongoUpdateField)  => x *> y
    case (x: MongoUpdateField,   y: MongoUpdateFields) => x *> toChanges(y)
    
    case (x: MongoUpdateFields, y: MongoUpdateField) => x *> y
    case (x: MongoUpdateFields, y: MongoUpdateFields)  => x *> toChanges(y)

    case (x: MongoUpdateObject, _)   => toUpdateFieldsValues(x) & that
    case (_, y: MongoUpdateObject)   => self & toUpdateFieldsValues(y)

    case (MongoUpdateNothing, _)  => that
    case (_, MongoUpdateNothing)  => self
  }
}

sealed case class MongoUpdateObject(value: JObject) extends MongoUpdate{
    def toJValue = value
}

object MongoUpdateObject{
  def decompose(jObject: JObject): List[MongoUpdateField] = decompose(jObject, None)

  private def decompose(jObject: JObject, parentPath: Option[JPath]): List[MongoUpdateField] = {
    jObject.fields.map(field => {
      val fieldPath = parentPath.map(_ \ field.name).getOrElse(JPath(field.name))

      jvalueToMongoPrimitive(field.value) match {
        case Some(MongoPrimitiveJObject(x)) => decompose(x, Some(fieldPath))
        case Some(v)                        => List(fieldPath.set(v))
        case None                           => List(fieldPath.unset)
      }
    }).flatten
  }
}

case object MongoUpdateNothing extends MongoUpdate{
  def toJValue: JObject = JObject(Nil)
}

sealed case class MongoUpdateFields(list: List[MongoUpdateField]) extends MongoUpdate{
  def toJValue: JObject = list.foldLeft(JObject(Nil)) { (obj, e) => obj.merge(e.toJValue).asInstanceOf[JObject] }
}

sealed trait MongoUpdateField extends MongoUpdate with Change1{  self =>
  def toJValue: JObject = JObject(JField(operator.symbol, JObject(JField(JPathExtension.toMongoField(path), filter.filter) :: Nil)) :: Nil)

  def operator: MongoUpdateOperator
  def filter: MongoFilter
}

case class IncF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $inc
}

case class SetF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $set

  override protected def fuseWithImpl(older: Change1) = Some(this)
}

case class UnsetF(path: JPath) extends MongoUpdateField{
  val operator = $unset
  val filter   = "" === MongoPrimitiveInt(1)
}

case class PopLastF(path: JPath) extends MongoUpdateField{
  val operator = $pop
  val filter   = "" === MongoPrimitiveInt(1)
}

case class PopFirstF(path: JPath) extends MongoUpdateField{
  val operator = $pop
  val filter   = ("" === MongoPrimitiveInt(-1))
}

case class PushF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $push
}

case class PullF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $pull
}

case class PushAllF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $pushAll
}

case class PullAllF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $pullAll
}

case class AddToSetF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
  val operator = $addToSet
}
