package blueeyes.core.storeable

sealed trait Record5[T1, T2, T3, T4, T5] extends Product with Storeable{ self =>
  def companion: Record5Companion[_ <: Record5[T1, T2, T3, T4, T5], T1, T2, T3, T4, T5]
}

trait Record5Companion[R <: Record5[T1, T2, T3, T4, T5], T1, T2, T3, T4, T5] extends Product5[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5]] with Companion[R]