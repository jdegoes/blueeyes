package blueeyes.json.serialization

import blueeyes.json._

import scalaz.{Validation, Success, Failure, NonEmptyList, Kleisli, Plus, Functor}
import Validation._
import NonEmptyList._
import scalaz.syntax.bifunctor._

/** Extracts the value from a JSON object. You must implement either validated or extract.
 */
trait Extractor[A] { self =>
  import Extractor._

  def extract(jvalue: JValue): A = validated(jvalue) valueOr { 
    case Thrown(ex) => throw new IllegalArgumentException("Unable to deserialize " + jvalue, ex)
    case other => throw new IllegalArgumentException("Unable to deserialize " + jvalue + ": " + other.message)
  }

  def validated(jvalue: JValue): Validation[Error, A] 
  def validated(jvalue: JValue, jpath: JPath): Validation[Error, A] =
    ((cause: Extractor.Error) => Extractor.Invalid("Unable to deserialize property or child " + jpath, Some(cause))) <-: validated(jvalue.get(jpath)) 

  def project(jpath: JPath): Extractor[A] = new Extractor[A] {
    override def extract(jvalue: JValue) = self.extract(jvalue(jpath))
    def validated(jvalue: JValue) = self.validated(jvalue(jpath))
  }

  def map[B](f: A => B): Extractor[B] = new Extractor[B] {
    override def extract(jvalue: JValue): B = f(self.extract(jvalue))
    def validated(jvalue: JValue) = self.validated(jvalue) map f
  }

  def mapv[B](f: A => Validation[Error, B]): Extractor[B] = new Extractor[B] {
    def validated(jvalue: JValue) = self.validated(jvalue) flatMap f
  }

  def kleisli = Kleisli[({type λ[+α] = Validation[Error, α]})#λ, JValue, A](validated _)

  def apply(jvalue: JValue): A = extract(jvalue)
}

object Extractor {
  def apply[A](implicit e: Extractor[A]): Extractor[A] = e

  def invalidv[A](message: String) = failure[Error, A](Invalid(message))
  def tryv[A](a: => A) = (Thrown.apply _) <-: Validation.fromTryCatch(a)

  sealed trait Error {
    def message: String
  }

  object Error {
    def thrown(exception: Throwable): Error = Thrown(exception)
    def invalid(message: String): Error = Invalid(message)
    def combine(errors: NonEmptyList[Error]): Error = Errors(errors)

    implicit object Semigroup extends scalaz.Semigroup[Error] {
      import NonEmptyList._
      def append(e1: Error, e2: => Error): Error = (e1, e2) match {
        case (Errors(l1), Errors(l2)) => Errors(l1.list <::: l2)
        case (Errors(l), x) => Errors(nel(x, l.list))
        case (x, Errors(l)) => Errors(x <:: l)
        case (x, y) => Errors(nels(x, y))
      }
    }
  }

  case class Invalid(msg: String, cause: Option[Error] = None) extends Error {
    def message = cause map { c => "%s (caused by %s)".format(msg, c.message) } getOrElse msg
  }

  case class Thrown(exception: Throwable) extends Error {
    def message: String = exception.getMessage
  }

  case class Errors(errors: NonEmptyList[Error]) extends Error {
    def message = "Multiple extraction errors occurred: " + errors.map(_.message).list.mkString(": ")
  }

  implicit val typeclass: Plus[Extractor] with Functor[Extractor] = new Plus[Extractor] with Functor[Extractor] {
    def plus[A](f1: Extractor[A], f2: => Extractor[A]) = new Extractor[A] {
      def validated(jvalue: JValue) = f1.validated(jvalue) findSuccess f2.validated(jvalue)
    }

    def map[A, B](e: Extractor[A])(f: A => B): Extractor[B] = e map f
  }

  def apply[A: Manifest](f: PartialFunction[JValue, A]): Extractor[A] = new Extractor[A] {
    def validated(jvalue: JValue) = {
      if (f.isDefinedAt(jvalue)) Success(f(jvalue)) 
      else Failure(Invalid("Extraction not defined from value " + jvalue + " to type " + implicitly[Manifest[A]].erasure.getName))
    }
  }
}
