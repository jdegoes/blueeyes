package blueeyes.core.storeable

trait Record6[T1, T2, T3, T4, T5, T6] extends Product with Record[Record6Companion[_, _, T1, T2, T3, T4, T5, T6]]

trait Record6Companion[P <: Record[_], R <: Record6[T1, T2, T3, T4, T5, T6], T1, T2, T3, T4, T5, T6] extends Product6[Field[R, T1], Field[R, T2], Field[R, T3], Field[R, T4], Field[R, T5], Field[R, T6]] with Companion[P, R]