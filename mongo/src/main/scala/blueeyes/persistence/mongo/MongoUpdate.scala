package blueeyes.persistence.mongo

import blueeyes.util.ProductPrefixUnmangler
import blueeyes.json.JPath
import blueeyes.json.JsonAST.{JField, JObject}
import MongoImplicits._
import MongoFilterOperators._

import com.mongodb.MongoException

import scala.math.BigInt._

import scalaz.Scalaz._


object MongoUpdateOperators {
  sealed trait MongoUpdateOperator extends Product with ProductPrefixUnmangler {
    lazy val symbol: String = unmangledName

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
  def inc(value: MongoPrimitive)        : MongoUpdate = IncF(jpath, value)
  def set(value: MongoPrimitive)        : MongoUpdate = SetF(jpath, value)
  def unset                             : MongoUpdate = UnsetF(jpath)
  def popLast                           : MongoUpdate = PopLastF(jpath)
  def popFirst                          : MongoUpdate = PopFirstF(jpath)
  def push [T](value: MongoPrimitive)   : MongoUpdate = PushF(jpath, value)
  def pull(filter: MongoFilter)         : MongoUpdate = PullF(jpath, filter)
  def pushAll [T <: MongoPrimitive](items: T*) : MongoUpdate = PushAllF(jpath, List(items: _*))
  def pullAll [T <: MongoPrimitive](items: T*) : MongoUpdate = PullAllF(jpath, List(items: _*))

  def addToSet [T <: MongoPrimitive](items: T*): MongoUpdateField = {
    val itemsList = List(items: _*)
    val (value, filter) = if (itemsList.size == 1) {
      val value: MongoPrimitive = itemsList.head
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
sealed trait MongoUpdate { self =>
  def toJValue: JObject;
}

object MongoUpdate {
  val Empty: MongoUpdate = MongoUpdateNothing
}

sealed case class MongoUpdateObject(value: JObject) extends MongoUpdate {
  def toJValue = value
}

private[mongo] object MongoUpdateObject {
  def decompose(jObject: JObject): Seq[MongoUpdateField] = decompose(jObject, None)

  private def decompose(jObject: JObject, parentPath: Option[JPath]): Seq[MongoUpdateField] = {
    jObject.fields.map(field => {
      val fieldPath = parentPath.map(_ \ field.name).getOrElse(JPath(field.name))

      jvalueToMongoPrimitive(field.value) match {
        case MongoPrimitiveJObject(x)   => decompose(x, Some(fieldPath))
        case v                          => Set(fieldPath.set(v).asInstanceOf[MongoUpdateField])
      }
    }).flatten
  }
}

case object MongoUpdateNothing extends MongoUpdate{
  def toJValue: JObject = JObject(Nil)
}

sealed case class MongoUpdateFields(list: Seq[MongoUpdateField]) extends MongoUpdate{
  private lazy val normalized = Set(list: _*)
  def toJValue: JObject = list.distinct.map(_.toJValue).toList.suml

  override def equals(that: Any): Boolean = that match {
    case that: MongoUpdateFields if (this.normalized.size == that.normalized.size) => normalized == that.normalized
    case _ => false
  }

  override lazy val hashCode = normalized.hashCode
}

sealed trait MongoUpdateField extends MongoUpdate with Change1{  self =>
  def toJValue: JObject = JObject(JField(operator.symbol, JObject(JField(JPathExtension.toMongoField(path), filter.filter) :: Nil)) :: Nil)

  def operator: MongoUpdateOperator
  def filter: MongoFilter
}

private[mongo] object UpdateFieldFunctions{
  case class SetF(path: JPath, value: MongoPrimitive) extends MongoUpdateField{
    val operator = $set

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = Some(this)
  }

  case class UnsetF(path: JPath) extends MongoUpdateField{
    val operator = $unset

    val filter   = "" === MongoPrimitiveInt(1)

    override protected def fuseWithImpl(older: Change1) = Some(this)
  }

  case class IncF(path: JPath, value: MongoPrimitive) extends MongoUpdateField{
    val operator = $inc

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v) => Some(SetF(path, plus(v, value)))

      case IncF(f, v) => Some(IncF(path, plus(v, value)))

      case _ => sys.error("IncF can be only combined with SetF and IncF. Older=" + older)
    }

    private def plus(v1: MongoPrimitive, v2: MongoPrimitive): MongoPrimitive = (v1, v2) match {
      case (MongoPrimitiveInt(x1),     MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveInt(x1),     MongoPrimitiveDouble(x2)) => x1 + x2
      case (MongoPrimitiveInt(x1),     MongoPrimitiveLong(x2))   => x1 + x2

      case (MongoPrimitiveDouble(x1),  MongoPrimitiveDouble(x2)) => x1 + x2
      case (MongoPrimitiveDouble(x1),  MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveDouble(x1),  MongoPrimitiveLong(x2))   => x1 + x2

      case (MongoPrimitiveLong(x1),    MongoPrimitiveLong(x2))   => x1 + x2
      case (MongoPrimitiveLong(x1),    MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveLong(x1),    MongoPrimitiveDouble(x2)) => x1 + x2

      case (MongoPrimitiveBigInt(x1),    MongoPrimitiveBigInt(x2))   => x1 + x2
      case (MongoPrimitiveBigInt(x1),    MongoPrimitiveInt(x2))    => x1 + x2
      case (MongoPrimitiveBigInt(x1),    MongoPrimitiveDouble(x2)) => x1.doubleValue + x2

      case _ => throw new MongoException("Modifier $inc allowed for numbers only")
    }
  }

  case class PopLastF(path: JPath) extends MongoUpdateField{
    val operator = $pop

    val filter   = "" === MongoPrimitiveInt(1)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)       => Some(SetF(path, pop(v)))

      case PopLastF(_) | PopFirstF(_)  => Some(this)

      case _ => sys.error("PopLastF can be only combined with SetF, PopLastF and PopFirstF. Older=" + older)
    }

    private def pop(v1: MongoPrimitive): MongoPrimitive  = v1 match{
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

      case _ => sys.error("PopLastF can be only combined with SetF, PopLastF and PopFirstF. Older=" + older)
    }

    private def pop(v1: MongoPrimitive): MongoPrimitive  = v1 match{
      case MongoPrimitiveArray(Nil) => v1
      case MongoPrimitiveArray(x)   => MongoPrimitiveArray(x.drop(1))

      case _ => throw new MongoException("Cannot apply $pop modifier to non-array")
    }
  }

  case class PushF(path: JPath, value: MongoPrimitive) extends MongoUpdateField{
    val operator = $push

    val filter   = ("" === value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)   => Some(SetF(path, push(v, value)))

      case PushF(_, olderValue)     => Some(PushAllF(path, value :: List(olderValue)))
      case PushAllF(_, olderValue)  => Some(PushAllF(path, value :: olderValue))

      case _ => sys.error("PushF can be only combined with SetF, PushAllF and PushF. Older=" + older)
    }

    private def push(v1: MongoPrimitive, v2: MongoPrimitive): MongoPrimitive  = v1 match{
      case MongoPrimitiveNull      => MongoPrimitiveArray(v2 :: Nil)
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x :+ v2)

      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }

  case class PushAllF(path: JPath, value: List[MongoPrimitive]) extends MongoUpdateField{
    val operator = $pushAll

    val filter   = "" === MongoPrimitiveArray(value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)       => Some(SetF(path, pushAll(v, value)))

      case PushAllF(_, olderValue)  => Some(PushAllF(path, value ::: olderValue))
      case PushF(_, olderValue)     => Some(PushAllF(path, value ::: List(olderValue)))

      case _ => sys.error("PushAllF can be only combined with SetF, PushF and PushAllF. Older=" + older)
    }

    private def pushAll(v1: MongoPrimitive, v2: List[MongoPrimitive]): MongoPrimitive  = v1 match{
      case MongoPrimitiveNull      => MongoPrimitiveArray(v2)
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x ++ v2)

      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }
  }

  case class PullAllF(path: JPath, value: List[MongoPrimitive]) extends MongoUpdateField{
    val operator = $pullAll

    val filter   = "" === MongoPrimitiveArray(value)

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)     => Some(SetF(path, pullAll(v, value)))

      case PullAllF(_, olderValue)       => Some(PullAllF(path, value ::: olderValue))
      case PullF(_, e: MongoFieldFilter) if (e.lhs == JPath("") && e.operator == $eq) => Some(PullAllF(path, value ::: List(e.rhs)))

      case _ => sys.error("""PullAllF can be only combined with SetF, PullAllF and PullF(when filter is ("" === value)). Older=""" + older)
    }

    private def pullAll(v1: MongoPrimitive, v2: List[MongoPrimitive]): MongoPrimitive  = v1 match{
      case MongoPrimitiveArray(x)  => MongoPrimitiveArray(x filterNot (v2 contains))

      case _ => throw new MongoException("Cannot apply $pullAll modifier to non-array")
    }
  }

  case class AddToSetF(path: JPath, value: MongoPrimitive, filter: MongoFilter) extends MongoUpdateField{
    val operator = $addToSet

    override protected def fuseWithImpl(older: Change1) = older match {
      case SetF(f, v)         => Some(SetF(path, addToSet(v, value)))

      case AddToSetF(_, olderValue, _) => {
        (olderValue, value) match {
          case (MongoPrimitiveArray(x), MongoPrimitiveArray(y)) => Some(path.addToSet(y ::: x :_*))
          case (MongoPrimitiveArray(x), y)                      => Some(path.addToSet(y :: x :_*))
          case (x, MongoPrimitiveArray(y))                      => Some(path.addToSet((y ::: List(x)) :_*))
          case (x, y) if (x == y)                               => Some(this)
          case (x, y)                                           => Some(path.addToSet(y, x))
        }
      }

      case _ => sys.error("PullAllF can be only combined with SetF and PullAllF. Older=" + older)
    }

    private def addToSet(v1: MongoPrimitive, v2: MongoPrimitive): MongoPrimitiveArray = v1 match {
      case MongoPrimitiveNull  => v2 match {
        case e: MongoPrimitiveArray  => e
        case MongoPrimitiveNull         => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _                          => MongoPrimitiveArray(v2 :: Nil)
      }
      case e: MongoPrimitiveArray => v2 match {
        case y: MongoPrimitiveArray  => {
          val initialArray: List[MongoPrimitive] = e.value

          MongoPrimitiveArray(y.value.foldLeft(initialArray) {(result, element) => addToSet0(result, element)})
        }
        case MongoPrimitiveNull         => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
        case _                          => MongoPrimitiveArray(addToSet0(e.value, v2))
      }
      case _ => throw new MongoException("Cannot apply $push/$pushAll modifier to non-array")
    }

    private def addToSet0(value: List[MongoPrimitive], setValue: MongoPrimitive) = value.find(_ == setValue) match {
      case Some(x) => value
      case None => value :+ setValue
    }
  }

  case class PullF(path: JPath, filter: MongoFilter) extends MongoUpdateField{
    import MongoFilterEvaluator._

    val operator = $pull

    override protected def fuseWithImpl(older: Change1) = (filter, older) match {
      case (_, SetF(f, v))         => Some(SetF(path, pull(v, filter)))

      case (x: MongoFieldFilter, PullF(_, y: MongoFieldFilter)) if (x.lhs == JPath("") && x.operator == $eq && y.lhs == JPath("") && y.operator == $eq) => Some(PullAllF(path, List(x.rhs, y.rhs)))
      case (x: MongoFieldFilter, PullAllF(_, oldValue)) if (x.lhs == JPath("") && x.operator == $eq) => Some(PullAllF(path, x.rhs :: oldValue))

      case (_, _) => sys.error("""PullF can be only combined with SetF PullAllF (when self.filter is ("" === value)) and PullF (when self.filter is ("" === value) and older.filter is ("" === value)). Older=""" + older)
    }

    private def pull(v1: MongoPrimitive, filter: MongoFilter): MongoPrimitive  = v1 match{
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
