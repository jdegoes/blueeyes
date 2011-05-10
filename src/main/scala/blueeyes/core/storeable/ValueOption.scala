package blueeyes.core.storeable

case class ValueOption[T](value: Option[T])(implicit valueToStoreable: T => Storeable) extends Value