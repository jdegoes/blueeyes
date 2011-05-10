package blueeyes.core.storeable

import scalaz.{Validation, Success}

trait Companion[R <: Product] extends Equals{ self =>
  def canEqual(that: Any) = that match{
    case x: AnyRef => this.getClass == x.getClass
    case _ => false
  }

  def _typename = _example.getClass.getName

  def _example: R

  implicit def valueToValidation[T](v: T) = Success(v)
}

case class Field[R, T] (name: String, getter: R => T, setter: (R, T) => Validation[String, R], default: Validation[String, T])(implicit fieldToStoreable: T => Storeable)