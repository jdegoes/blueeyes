package blueeyes.core.storeable

trait Record2[T1, T2] extends Product with Record[Record2Companion[_, _, T1, T2]]

trait Record2Companion[P <: Record[_], R <: Record2[T1, T2], T1, T2] extends Product2[Field[R, T1], Field[R, T2]] with Companion[P, R]
