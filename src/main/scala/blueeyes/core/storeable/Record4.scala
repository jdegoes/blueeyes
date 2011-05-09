package blueeyes.core.storeable

sealed trait Record4[T1, T2, T3, T4] extends Product with Record{ self =>
  def companion: Record4Companion[_ <: Record4[T1, T2, T3, T4], T1, T2, T3, T4]
}

trait Record4Companion[R <: Record4[T1, T2, T3, T4], T1, T2, T3, T4] extends Product4[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4]] with Companion[R]