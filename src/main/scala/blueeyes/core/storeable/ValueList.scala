package blueeyes.core.storeable

case class ValueList[T <: Storeable](value: List[T]) extends Value