package blueeyes.core.data

import blueeyes.json.JValue
import scala.xml.NodeSeq

import scalaz.{ Validation, Failure, ValidationNEL }
import scalaz.Scalaz._

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
  def as[B](implicit f: Biject.E[A, B]): B = f.fold(_ apply a, _ unapply a)
}

object Biject {
  type E[A, B] = Either[Function1[A,B], Unapply[B,A]]
}

trait Bijections {
  implicit def identity[T]: Bijection[T, T] = new Bijection[T, T] {
    def apply(t: T): T = t
    def unapply(t: T): T = t
  }

  implicit def biject[A](a: A): Biject[A] = new Biject(a)
  implicit def forwardEither[A, B](implicit a: Function1[A,B]): Biject.E[A, B] = Left(a)
  implicit def reverseEither[A, B](implicit b: Unapply[B,A]): Biject.E[A, B] = Right(b)
}

trait IdentityBijections extends Bijections {
  implicit val JValueToJValue         = identity[JValue]
  implicit val StringToString         = identity[String]
  implicit val ArrayByteToArrayByte   = identity[Array[Byte]]
  implicit val XMLToXML               = identity[NodeSeq]
  implicit val ByteChunkToByteChunk   = identity[ByteChunk]
}

object Bijection extends Bijections with IdentityBijections 

trait PartialBijection[A, B] extends Function1[A, ValidationNEL[String, B]] with Unapply[ValidationNEL[String, A], B] { self =>
  //TODO: make this not discard errors
  def compose[C](next: PartialBijection[C, A]): PartialBijection[C, B] = new PartialBijection[C, B] {
    def apply(c: C) = next(c).flatMap(self) 
    def unapply(b: B) = self.unapply(b).flatMap(next.unapply)
  }

  //TODO: make this not discard errors
  def andThen[C](next: PartialBijection[B, C]): PartialBijection[A, C] = new PartialBijection[A, C] {
    def apply(a: A) = self(a).flatMap(next)
    def unapply(c: C) = next.unapply(c).flatMap(self.unapply)
  }

  def orElse(other: PartialBijection[A, B]): PartialBijection[A, B] = new PartialBijection[A, B] {
    def apply(a: A) = self(a) match {
      case Failure(_) => other(a)
      case success => success
    }

    def unapply(b: B) = self.unapply(b) match {
      case Failure(_) => other.unapply(b)
      case success => success
    }
  }

  def inverse: PartialBijection[B, A] = new PartialBijection[B, A] {
    def apply(b: B) = self.unapply(b)
    def unapply(a: A) = self(a)
  }
}

sealed class PartialBiject[A](a: A) {
  def as[B](implicit f: PartialBiject.E[A, B]): ValidationNEL[String, B] = f.fold(_ apply a, _ unapply a)
}

object PartialBiject {
  type E[A, B] = Either[Function1[A, ValidationNEL[String, B]], Unapply[ValidationNEL[String, B], A]] 
}

object PartialBijections {
  implicit def identity[T]: PartialBijection[T, T] = new PartialBijection[T, T] {
    def apply(t: T) = t.success
    def unapply(t: T) = t.success
  }

  implicit def partialBiject[A](a: A): PartialBiject[A] = new PartialBiject(a)
  implicit def forwardEither[A, B](implicit a: Function1[A, ValidationNEL[String, B]]): PartialBiject.E[A, B]= Left(a)
  implicit def reverseEither[A, B](implicit b: Unapply[ValidationNEL[String, B], A]): PartialBiject.E[A, B] = Right(b)
}

