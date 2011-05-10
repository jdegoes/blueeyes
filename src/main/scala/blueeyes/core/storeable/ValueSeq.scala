package blueeyes.core.storeable

case class ValueSeq[T](value: Seq[T])(implicit valueToStoreable: T => Storeable) extends Value