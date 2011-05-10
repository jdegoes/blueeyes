package blueeyes.core.storeable

trait Record8[T1, T2, T3, T4, T5, T6, T7, T8] extends Product with Record{ self =>
  def companion: Record8Companion[_ <: Record8[T1, T2, T3, T4, T5, T6, T7, T8], T1, T2, T3, T4, T5, T6, T7, T8]
}

trait Record8Companion[R <: Record8[T1, T2, T3, T4, T5, T6, T7, T8], T1, T2, T3, T4, T5, T6, T7, T8] extends Product8[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8]] with Companion[R] with ValueImplicits