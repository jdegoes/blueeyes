package blueeyes.core.data

sealed trait Unapply[A, B] {
  def unapply(b: B): A
}

trait Bijection[A, B] extends Function1[A, B] with Unapply[A, B] { self =>
  def isDefinedAt(t: A): Boolean = {
    try {
      apply(t)
      true
    } catch {
      case ex => false
    }
  }
  
  def apply(t: A): B
  
  def inverse: Bijection[B, A] = new Bijection[B, A] {
    def apply(s: B): A   = self.unapply(s)
    def unapply(t: A): B = self.apply(t)
  }
  
  def compose[R](that: Bijection[R, A]): Bijection[R, B] = new Bijection[R, B] {
    def apply(r: R): B = self.apply(that.apply(r))
    def unapply(s: B): R = that.unapply(self.unapply(s))
  }
  
  def andThen[R](that: Bijection[B, R]): Bijection[A, R] = that.compose(self)
}

sealed class Biject[A](a: A) {
  def as[B](implicit f: Either[Function1[A, B], Unapply[B, A]]): B = f.fold(_ apply a, _ unapply a)
}

trait Bijections {
  implicit def biject[A](a: A): Biject[A] = new Biject(a)
  implicit def forwardEither[A, B](implicit a: Function1[A,B]): Either[Function1[A,B], Unapply[B,A]] = Left(a)
  implicit def reverseEither[A, B](implicit b: Unapply[B,A]): Either[Function1[A,B], Unapply[B,A]] = Right(b)
}

object Bijection extends Bijections with BijectionsIdentity
