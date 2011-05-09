package blueeyes.core.storeable

sealed trait Record9[T1, T2, T3, T4, T5, T6, T7, T8, T9] extends Product with Record{ self =>
  def companion: Record9Companion[_ <: Record9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T1, T2, T3, T4, T5, T6, T7, T8, T9]
}

trait Record9Companion[R <: Record9[T1, T2, T3, T4, T5, T6, T7, T8, T9], T1, T2, T3, T4, T5, T6, T7, T8, T9] extends Product9[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9]] with Companion[R]