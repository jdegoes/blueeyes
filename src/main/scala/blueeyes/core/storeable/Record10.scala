package blueeyes.core.storeable

trait Record10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] extends Product with Record{ self =>
  def companion: Record10Companion[_ <: Record10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]
}

trait Record10Companion[R <: Record10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] extends Product10[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10]] with Companion[R] with ValueImplicits