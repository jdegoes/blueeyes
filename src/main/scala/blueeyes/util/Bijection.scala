package blueeyes.core.service

trait Bijection[T, S] extends Function1[T, S] { self =>
  def apply(t: T): S
  
  def unapply(s: S): T
  
  def inverse: Bijection[S, T] = new Bijection[S, T] {
    def apply(s: S): T   = self.unapply(s)
    def unapply(t: T): S = self.apply(t)
  }
}

trait DataTranscoder[T, S] {
  def transcode: Bijection[T, S]
}