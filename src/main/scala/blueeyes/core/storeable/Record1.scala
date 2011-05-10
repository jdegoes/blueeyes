package blueeyes.core.storeable

trait Record1[T1] extends Record[Record1Companion[_, _, T1]] with Product

trait Record1Companion[P <: Record[_], R <: Record1[T1], T1] extends Product1[Field[R, T1]] with Companion[P, R]
