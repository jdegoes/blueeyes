package blueeyes.core.storeable

import blueeyes.json.JsonAST.JValue
import org.joda.time.DateTime
import scalaz.{Validation, Success, Failure}

sealed trait Storeable

sealed trait Record[C <: Companion[_, _]] extends Storeable {
  def companion: C
}

trait Companion[P <: Record[_], R <: Record[_]] extends Equals with ValueImplicits{ self =>
  def canEqual(that: Any) = that match{
    case x: AnyRef => this.getClass == x.getClass
    case _ => false
  }

  def _typename = _example.getClass.getName

  def _example: R

  def _upgrade(old: P): Validation[String, R]   = Failure("There is nothing to upgrade")

  def _downgrade(cur: R): Validation[String, P] = Failure("There is nothing to downgrade")

  lazy val _version: Int = _downgrade(_example)  match {case Failure(_) => 1; case Success(old) => old.companion.asInstanceOf[Companion[_, _]]._version}

  implicit def valueToValidation[T](v: T) = Success(v)
}

case class Field[R, T] (name: String, getter: R => T, setter: (R, T) => Validation[String, R], default: Validation[String, T])(implicit fieldToStoreable: T => Storeable)

case object RecordNothing extends Record0{
  def companion = RecordNothingCompanion
}
case object RecordNothingCompanion extends RecordCompanion0[RecordNothing.type, RecordNothing.type]{
  def _example = RecordNothing
}

trait Record0 extends Product with Record[RecordCompanion0[_, _]]

trait RecordCompanion0[P <: Record[_], R <: Record0] extends Product with Companion[P, R]

trait Record1[T1] extends Record[Record1Companion[_, _, T1]] with Product
trait Record1Companion[P <: Record[_], R <: Record1[T1], T1] extends Product1[Field[R, T1]] with Companion[P, R]

trait Record2[T1, T2] extends Product with Record[Record2Companion[_, _, T1, T2]]
trait Record2Companion[P <: Record[_], R <: Record2[T1, T2], T1, T2] extends Product2[Field[R, T1], Field[R, T2]] with Companion[P, R]

trait Record3[T1, T2, T3] extends Product with Record[Record3Companion[_, _, T1, T2, T3]]
trait Record3Companion[P <: Record[_], R <: Record3[T1, T2, T3], T1, T2, T3] extends Product3[Field[R, T1], Field[R, T2], Field[R, T3]] with Companion[P, R]

trait Record4[T1, T2, T3, T4] extends Product with Record[Record4Companion[_, _, T1, T2, T3, T4]]
trait Record4Companion[P <: Record[_], R <: Record4[T1, T2, T3, T4], T1, T2, T3, T4] extends Product4[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4]] with Companion[P, R]

trait Record5[T1, T2, T3, T4, T5] extends Product with Record[Record5Companion[_, _, T1, T2, T3, T4, T5]]
trait Record5Companion[P <: Record[_], R <: Record5[T1, T2, T3, T4, T5], T1, T2, T3, T4, T5] extends Product5[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5]] with Companion[P, R]

trait Record6[T1, T2, T3, T4, T5, T6] extends Product with Record[Record6Companion[_, _, T1, T2, T3, T4, T5, T6]]
trait Record6Companion[P <: Record[_], R <: Record6[T1, T2, T3, T4, T5, T6], T1, T2, T3, T4, T5, T6] extends Product6[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6]] with Companion[P, R]

trait Record7[T1, T2, T3, T4, T5, T6, T7] extends Product with Record[Record7Companion[_, _, T1, T2, T3, T4, T5, T6, T7]]
trait Record7Companion[P <: Record[_], R <: Record7[T1, T2, T3, T4, T5, T6, T7], T1, T2, T3, T4, T5, T6, T7] extends Product7[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7]] with Companion[P, R]

trait Record8[T1, T2, T3, T4, T5, T6, T7, T8] extends Product with Record[Record8Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8]]
trait Record8Companion[P <: Record[_], R <: Record8[T1, T2, T3, T4, T5, T6, T7, T8], T1, T2, T3, T4, T5, T6, T7, T8] extends Product8[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8]] with Companion[P, R]

trait Record9[T1, T2, T3, T4, T5, T6, T7, T8, T9] extends Product with Record[Record9Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9]]
trait Record9Companion[P <: Record[_], R <: Record9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T1, T2, T3, T4, T5, T6, T7, T8, T9] extends Product9[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9]] with Companion[P, R]

trait Record10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] extends Product with Record[Record10Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]]
trait Record10Companion[P <: Record[_], R <: Record10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] extends Product10[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10]] with Companion[P, R]

trait Record11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] extends Product with Record[Record11Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]]
trait Record11Companion[P <: Record[_], R <: Record11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] extends Product11[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11]] with Companion[P, R]

trait Record12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] extends Product with Record[Record12Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]]
trait Record12Companion[P <: Record[_], R <: Record12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12]
    extends Product12[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12]] with Companion[P, R]

trait Record13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] extends Product with Record[Record13Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]]
trait Record13Companion[P <: Record[_], R <: Record13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    extends Product13[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13]] with Companion[P, R]

trait Record14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] extends Product with Record[Record14Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]]
trait Record14Companion[P <: Record[_], R <: Record14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14]
    extends Product14[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14]] with Companion[P, R]

trait Record15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] extends Product with Record[Record15Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]
trait Record15Companion[P <: Record[_], R <: Record15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    extends Product15[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15]] with Companion[P, R]

trait Record16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] extends Product with Record[Record16Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]]
trait Record16Companion[P <: Record[_], R <: Record16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16]
    extends Product16[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16]] with Companion[P, R]

trait Record17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] extends Product with Record[Record17Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]]
trait Record17Companion[P <: Record[_], R <: Record17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17]
    extends Product17[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17]] with Companion[P, R]

trait Record18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] extends Product with Record[Record18Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]]
trait Record18Companion[P <: Record[_], R <: Record18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18]
    extends Product18[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17], Field[R, T18]] with Companion[P, R]

trait Record19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] extends Product with Record[Record19Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]]
trait Record19Companion[P <: Record[_], R <: Record19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19]
    extends Product19[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17], Field[R, T18], Field[R, T19]] with Companion[P, R]

trait Record20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] extends Product with Record[Record20Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]]
trait Record20Companion[P <: Record[_], R <: Record20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20]
    extends Product20[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17], Field[R, T18], Field[R, T19], Field[R, T20]] with Companion[P, R]

trait Record21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] extends Product with Record[Record21Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]]
trait Record21Companion[P <: Record[_], R <: Record21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]
    extends Product21[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17], Field[R, T18], Field[R, T19], Field[R, T20], Field[R, T21]] with Companion[P, R]

trait Record22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] extends Product with Record[Record22Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]]
trait Record22Companion[P <: Record[_], R <: Record22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22]
    extends Product22[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15], Field[R, T16], Field[R, T17], Field[R, T18], Field[R, T19], Field[R, T20], Field[R, T21], Field[R, T22]] with Companion[P, R]

sealed trait Value extends Storeable

case class ValueBigInt(value: BigInt) extends Value

case class ValueBigDecimal(value: BigDecimal) extends Value

case class ValueBoolean(value: Boolean) extends Value

case class ValueByte(value: Byte) extends Value

case class ValueChar(value: Char) extends Value

case class ValueDateTime(value: DateTime) extends Value

case class ValueDouble(value: Double) extends Value

case class ValueFloat(value: Float) extends Value

case class ValueInt(value: Int) extends Value

case class ValueLong(value: Long) extends Value

case class ValueShort(value: Short) extends Value

case class ValueString(value: String) extends Value

case class ValueJson(value: JValue) extends Value

case class ValueMap[K, V](value: Map[K, V])(implicit keyToStoreable: K => Storeable, valueToStoreable: V => Storeable) extends Value

case class ValueOption[T](value: Option[T])(implicit valueToStoreable: T => Storeable) extends Value

case class ValueSeq[T](value: Seq[T])(implicit valueToStoreable: T => Storeable) extends Value

case class ValueList[T](value: List[T])(implicit valueToStoreable: T => Storeable) extends Value

case class ValueArray[T](value: Array[T])(implicit valueToStoreable: T => Storeable) extends Value

case class ValueSet[T](value: Set[T])(implicit valueToStoreable: T => Storeable) extends Value

case class ValueTuple1[T1](value: Tuple1[T1])(implicit vToS: T1 => Storeable) extends Value

case class ValueTuple2[T1, T2](value: Tuple2[T1, T2])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable) extends Value

case class ValueTuple3[T1, T2, T3](value: Tuple3[T1, T2, T3])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable) extends Value

case class ValueTuple4[T1, T2, T3, T4](value: Tuple4[T1, T2, T3, T4])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable) extends Value

case class ValueTuple5[T1, T2, T3, T4, T5](value: Tuple5[T1, T2, T3, T4, T5])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable) extends Value

case class ValueTuple6[T1, T2, T3, T4, T5, T6](value: Tuple6[T1, T2, T3, T4, T5, T6])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable) extends Value

case class ValueTuple7[T1, T2, T3, T4, T5, T6, T7](value: Tuple7[T1, T2, T3, T4, T5, T6, T7])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable) extends Value

case class ValueTuple8[T1, T2, T3, T4, T5, T6, T7, T8](value: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable) extends Value

case class ValueTuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](value: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable) extends Value

case class ValueTuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](value: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable) extends Value

case class ValueTuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](value: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable) extends Value

case class ValueTuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](value: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable) extends Value

case class ValueTuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](value: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable) extends Value

case class ValueTuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](value: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable) extends Value

case class ValueTuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](value: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable) extends Value

case class ValueTuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](value: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable) extends Value

case class ValueTuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](value: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable) extends Value

case class ValueTuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](value: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable, v18ToS: T18 => Storeable) extends Value

case class ValueTuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](value: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable, v18ToS: T18 => Storeable, v19ToS: T19 => Storeable) extends Value

case class ValueTuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](value: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable, v18ToS: T18 => Storeable, v19ToS: T19 => Storeable, v20ToS: T20 => Storeable) extends Value

case class ValueTuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](value: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable, v18ToS: T18 => Storeable, v19ToS: T19 => Storeable, v20ToS: T20 => Storeable, v21ToS: T21 => Storeable) extends Value

case class ValueTuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](value: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22])(implicit v1ToS: T1 => Storeable, v2ToS: T2 => Storeable, v3ToS: T3 => Storeable, v4ToS: T4 => Storeable, v5ToS: T5 => Storeable, v6ToS: T6 => Storeable, v7ToS: T7 => Storeable, v8ToS: T8 => Storeable, v9ToS: T9 => Storeable, v10ToS: T10 => Storeable, v11ToS: T11 => Storeable, v12ToS: T12 => Storeable, v13ToS: T13 => Storeable, v14ToS: T14 => Storeable, v15ToS: T15 => Storeable, v16ToS: T16 => Storeable, v17ToS: T17 => Storeable, v18ToS: T18 => Storeable, v19ToS: T19 => Storeable, v20ToS: T20 => Storeable, v21ToS: T21 => Storeable, v22ToS: T22 => Storeable) extends Value

//import scalaz.{Failure, Success}
//case class Person(name: String, age: Int, orders: List[Person]) extends Record3[String, Int, List[Person]] {
//  def companion = Person
//}
//
//object Person extends Record3Companion[RecordNothing.type, Person, String, Int, List[Person]]{
//  val _1 = Field[Person, String]("name", _.name, (person, name) => person.copy(name = name), Failure("No name specified!"))
//
//  val _2 = Field[Person, Int]("age", _.age, (person, age) => person.copy(age = age), -1)
//
//  val _3 = Field[Person, List[Person]]("orders", _.orders, (person, orders) => person.copy(orders = orders), Failure("No name specified!"))
//
//  def _example: Person = Person("John Doe", 123, Nil)
//}