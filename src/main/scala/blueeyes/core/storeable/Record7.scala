package blueeyes.core.storeable

trait Record7[T1, T2, T3, T4, T5, T6, T7] extends Product with Record[Record7Companion[_, _, T1, T2, T3, T4, T5, T6, T7]]

trait Record7Companion[P <: Record[_], R <: Record7[T1, T2, T3, T4, T5, T6, T7], T1, T2, T3, T4, T5, T6, T7] extends Product7[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6], Field[R, T7]] with Companion[P, R]