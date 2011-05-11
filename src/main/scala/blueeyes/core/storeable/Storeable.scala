package blueeyes.core.storeable

import scalaz.{Validation, Success, Failure}

sealed trait Storeable

 trait Value extends Storeable

trait Record[C <: Companion[_, _]] extends Storeable {
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
