package blueeyes.core.storeable

sealed trait Record11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] extends Product with Storeable{ self =>
  def companion: Record11Companion[_ <: Record11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11]
}

trait Record11Companion[R <: Record11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] extends Product11[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11]] with Companion[R]