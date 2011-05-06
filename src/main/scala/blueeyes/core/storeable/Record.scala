package blueeyes.core.storeable

sealed trait Record extends Product with Storeable{
  def companion[T <: RecordCompanion[_ <: Record]]
}

trait RecordCompanion[R <: Record] extends Product with Companion[R]