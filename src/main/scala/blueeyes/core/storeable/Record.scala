package blueeyes.core.storeable

sealed trait Record0 extends Product with Record{
  def companion: RecordCompanion0[_ <: Record0]
}

trait RecordCompanion0[R <: Record0] extends Product with Companion[R]