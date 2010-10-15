package blueeyes.core.service

trait Bijection[T, S] {
  def apply(t: T): S
  
  def inverse: Bijection[S, T]
  
  def unapply(s: S): T = inverse.apply(s)
}

trait DataTranscoder[T, S] {
  def transcode: Bijection[T, S]
}