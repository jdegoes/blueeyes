package blueeyes.core.storeable

case class ValueSet[T](value: Set[T])(implicit valueToStoreable: T => Storeable) extends Value