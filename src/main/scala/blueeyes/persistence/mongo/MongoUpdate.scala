package blueeyes.persistence.mongo

import blueeyes.util.ProductPrefixUnmangler
import blueeyes.json.JsonAST.{JField, JObject}
import blueeyes.json.{JPath}
import MongoImplicits._
import MongoFilterOperators._
import com.mongodb.MongoException

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
import UpdateFieldFunctions._
/** The MongoUpdateBuilder creates update value for mongo update query.
 * <p>
 * <pre>
 * import blueeyes.persistence.mongo.MongoImplicits._
 * import blueeyes.persistence.mongo.MongoQueryBuilder._
 *
 * val value = "foo.bar" popFirst
 *
 * val query  = updateMany(collection).set(value)
 * val query2 = updateMany(collection).set("address" popFirst)
 * </pre>
 * <p>
 * Update values can be combined.
 * <p>
 * <pre>
 * val value   = ("foo" popLast) & ("bar" popFirst)
 * </pre>
 * <p>
 */
case class MongoUpdateBuilder(jpath: JPath) {
  def inc [T](value: MongoPrimitive[T]) : MongoUpdateField = IncF(jpath, value)
  def set [T](value: MongoPrimitive[T]) : MongoUpdateField = SetF(jpath, value)
  def unset                             : MongoUpdateField = UnsetF(jpath)
  def popLast                           : MongoUpdateField = PopLastF(jpath)
  def popFirst                          : MongoUpdateField = PopFirstF(jpath)
  def push [T](value: MongoPrimitive[T]): MongoUpdateField = PushF(jpath, value)
  def pull(filter: MongoFilter)         : MongoUpdateField = PullF(jpath, filter)
  def pushAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateField = PushAllF(jpath, List(items: _*))
  def pullAll [T <: MongoPrimitive[_]](items: T*) : MongoUpdateField = PullAllF(jpath, List(items: _*))

  def addToSet [T <: MongoPrimitive[_]](items: T*): MongoUpdateField = {
    val itemsList = List(items: _*)
    val (value, filter) = if (itemsList.size == 1) {
      val value: MongoPrimitive[_] = itemsList.head
      (value, "" === value)
    }
    else {
      val value = MongoPrimitiveArray(itemsList)
      (value, MongoFieldFilter(JPath(""), $each, value))
    }
    AddToSetF(jpath, value, filter)
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
    
    case (x: MongoUpdateFields, y: MongoUpdateField)   => x *> y
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

private[mongo] object MongoUpdateObject{
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

private[mongo] object UpdateFieldFunctions{
  case class SetF(path: JPath, value: MongoPrimitive[_]) extends MongoUpdateField{
    val operator = $set

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = Some(this)
  }

  case class UnsetF(path: JPath) extends MongoUpdateField{
    val operator = $unset

    val filter   = "" === MongoPrimitiveInt(1)

    override protected def fuseWithImpl(older: Change1) = Some(this)
  }

  case class IncF(path: JPath, value: MongoPrimitive[_]) extends MongoUpdateField{
    val operator = $inc

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v) => Some(SetF(path, plus(v, value)))

      case IncF(f, v) => Some(IncF(path, plus(v, value)))

      case _ => error("IncF can be only combined with SetF and IncF. Older=" + older)
    }

    private def plus(v1: MongoPrimitive[_], v2: MongoPrimitive[_]): MongoPrimitive[_] = (v1, v2) match {
      case (MongoPrimitiveInt(x1),     MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveInt(x1),     MongoPrimitiveDouble(x2)) => x1 + x2
      case (MongoPrimitiveInt(x1),     MongoPrimitiveLong(x2))   => x1 + x2

      case (MongoPrimitiveDouble(x1),  MongoPrimitiveDouble(x2)) => x1 + x2
      case (MongoPrimitiveDouble(x1),  MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveDouble(x1),  MongoPrimitiveLong(x2))   => x1 + x2

      case (MongoPrimitiveLong(x1),    MongoPrimitiveLong(x2))   => x1 + x2
      case (MongoPrimitiveLong(x1),    MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveLong(x1),    MongoPrimitiveDouble(x2)) => x1 + x2

      case _ => throw new MongoException("Modifier $inc allowed for numbers only")
    }
  }

  case class PopLastF(path: JPath) extends MongoUpdateField{
    val operator = $pop

    val filter   = "" === MongoPrimitiveInt(1)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)       => Some(SetF(path, pop(v)))

      case PopLastF(_) | PopFirstF(_)  => Some(this)

      case _ => error("PopLastF can be only combined with SetF, PopLastF and PopFirstF. Older=" + older)
    }

    private def pop(v1: MongoPrimitive[_]): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveArray(Nil) => v1
      case MongoPrimitiveArray(x)   => MongoPrimitiveArray(x.dropRight(1))

      case _ => throw new MongoException("Cannot apply $pop modifier to non-array")
    }
  }

  case class PopFirstF(path: JPath) extends MongoUpdateField{
    val operator = $pop

    val filter   = ("" === MongoPrimitiveInt(-1))

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)                 => Some(SetF(path, pop(v)))

      case PopLastF(_) | PopFirstF(_) => Some(this)

      case _ => error("PopLastF can be only combined with SetF, PopLastF and PopFirstF. Older=" + older)
    }

    private def pop(v1: MongoPrimitive[_]): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveArray(Nil) => v1
      case MongoPrimitiveArray(x)   => MongoPrimitiveArray(x.drop(1))

      case _ => throw new MongoException("Cannot apply $pop modifier to non-array")
    }
  }

  case class PushF(path: JPath, value: MongoPrimitive[_]) extends MongoUpdateField{
    val operator = $push

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)   => Some(SetF(path, push(v, value)))

      case PushF(_, _)  => Some(this)

      case _ => error("PushF can be only combined with SetF and PushF. Older=" + older)
    }

    private def push(v1: MongoPrimitive[_], v2: MongoPrimitive[_]): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveNull      => MongoPrimitiveArray(v2 :: Nil)
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x :+ v2)

      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }

  case class PushAllF[T <: MongoPrimitive[_]](path: JPath, value: List[T]) extends MongoUpdateField{
    val operator = $pushAll

    val filter   = "" === MongoPrimitiveArray(value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)       => Some(SetF(path, pushAll(v, value)))

      case PushAllF(_, _)  => Some(this)

      case _ => error("PushAllF can be only combined with SetF and PushAllF. Older=" + older)
    }

    private def pushAll(v1: MongoPrimitive[_], v2: List[T]): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveNull      => MongoPrimitiveArray(v2)
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x ++ v2)

      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }

  case class PullAllF[T <: MongoPrimitive[_]](path: JPath, value: List[T]) extends MongoUpdateField{
    val operator = $pullAll

    val filter   = "" === MongoPrimitiveArray(value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)     => Some(SetF(path, pullAll(v, value)))

      case PullAllF(_, _) => Some(this)

      case _ => error("PullAllF can be only combined with SetF and PullAllF. Older=" + older)
    }

    private def pullAll(v1: MongoPrimitive[_], v2: List[T]): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x filterNot (v2 contains))

      case _ => throw new MongoException("Cannot apply $pullAll modifier to non-array")
    }
  }

  case class AddToSetF(path: JPath, value: MongoPrimitive[_], filter: MongoFilter) extends MongoUpdateField{
    val operator = $addToSet

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)         => Some(SetF(path, addToSet(v, value)))

      case AddToSetF(_, _, _) => Some(this)

      case _ => error("PullAllF can be only combined with SetF and PullAllF. Older=" + older)
    }

    private def addToSet(v1: MongoPrimitive[_], v2: MongoPrimitive[_]): MongoPrimitiveArray[_]  = v1 match {
      case MongoPrimitiveNull  => v2 match {
        case e: MongoPrimitiveArray[_]  => e
        case MongoPrimitiveNull         => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _                          => MongoPrimitiveArray(v2 :: Nil)
      }
      case e: MongoPrimitiveArray[_] => v2 match {
        case y: MongoPrimitiveArray[_]  => {
          val initialArray: List[MongoPrimitive[_]] = e.value

          MongoPrimitiveArray(y.value.foldLeft(initialArray) {(result, element) => addToSet0(result, element)})
        }
        case MongoPrimitiveNull         => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _                          => MongoPrimitiveArray(addToSet0(e.value, v2))
      }
      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }

    private def addToSet0(value: List[MongoPrimitive[_]], setValue: MongoPrimitive[_]) = value.find(_ == setValue) match {
      case Some(x) => value
      case None => value :+ setValue
    }
  }

  case class PullF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
    import MongoFilterEvaluator._

    val operator = $pull

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)         => Some(SetF(path, pull(v, filter)))

      case PullF(_, _) => Some(this)

      case _ => error("PullF can be only combined with SetF and PullF. Older=" + older)
    }

    private def pull(v1: MongoPrimitive[_], filter: MongoFilter): MongoPrimitive[_]  = v1 match{
      case MongoPrimitiveArray(x)  => {
        val valuesAndJValues  = x.zip(x.map(_.toJValue))
        val matchedJVales     = valuesAndJValues.unzip._2.filter(filter)
        val filtered          = valuesAndJValues.filterNot(v => matchedJVales contains v._2).unzip._1

        MongoPrimitiveArray(filtered)
      }

      case _ => throw new MongoException("Cannot apply $pull modifier to non-array")
    }
  }
}