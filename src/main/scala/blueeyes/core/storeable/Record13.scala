package blueeyes.core.storeable

sealed trait Record13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] extends Product with Record{ self =>
  def companion: Record13Companion[_ <: Record13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
}

trait Record13Companion[R <: Record13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13]
    extends Product13[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13]] with Companion[R]