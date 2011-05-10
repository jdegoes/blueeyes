package blueeyes.core.storeable

trait Record15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] extends Product with Record[Record15Companion[_, _, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]]

trait Record15Companion[P <: Record[_], R <: Record15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15]
    extends Product15[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7], Field[R, T8], Field[R, T9], Field[R, T10], Field[R, T11], Field[R, T12], Field[R, T13], Field[R, T14], Field[R, T15]] with Companion[P, R]