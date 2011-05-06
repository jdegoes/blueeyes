package blueeyes.core.storeable

sealed trait Record3[T1, T2, T3] extends Product with Storeable{ self =>
  def companion: Record3Companion[_ <: Record3[T1, T2, T3], T1, T2, T3]
}

trait Record3Companion[R <: Record3[T1, T2, T3], T1, T2, T3] extends Product3[Field[R, T1], Field[R, T2], Field[R, T3]] with Companion[R]