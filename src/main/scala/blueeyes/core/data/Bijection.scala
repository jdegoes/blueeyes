package blueeyes.core.data

import blueeyes.json.JsonAST.JValue
import scala.xml.NodeSeq

sealed trait Unapply[A, B] {
  def unapply(b: B): A
}

object Unapply {
  implicit def identity[A]: Unapply[A, A] = new Unapply[A, A] {
    def unapply(a: A) = a
  }
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
  implicit def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t
    def unapply(t: T): T = t
  }

  implicit def biject[A](a: A): Biject[A] = new Biject(a)
  implicit def forwardEither[A, B](implicit a: Function1[A,B]): Either[Function1[A,B], Unapply[B,A]] = Left(a)
  implicit def reverseEither[A, B](implicit b: Unapply[B,A]): Either[Function1[A,B], Unapply[B,A]] = Right(b)
}

trait IdentityBijections extends Bijections {
  implicit val JValueToJValue         = identity[JValue]
  implicit val StringToString         = identity[String]
  implicit val ArrayByteToArrayByte   = identity[Array[Byte]]
  implicit val XMLToXML               = identity[NodeSeq]
  implicit val ByteChunkToByteChunk   = identity[ByteChunk]
}

object Bijection extends Bijections with IdentityBijections 
