package blueeyes.core.storeable

trait Record1[T1] extends Product with Storeable{
  def companion[T <: Record1Companion[_ <: Record1[T1], T1]]
}

trait Record1Companion[R <: Record1[T1], T1] extends Product1[Field[R, T1]] with Companion[R]
