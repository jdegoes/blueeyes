package blueeyes.core.storeable

case class ValueMap[K, V](value: Map[K, V])(implicit keyToStoreable: K => Storeable, valueToStoreable: V => Storeable) extends Value