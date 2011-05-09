package blueeyes.core.storeable

case class ValueOption[T <: Storeable](value: T) extends Value