package blueeyes.json.serialization

import blueeyes.json.JsonAST._

import scalaz.{Validation, Success, Failure, NonEmptyList, Kleisli}
import NonEmptyList._

/** Extracts the value from a JSON object. You must implement either validated or extract.
 */
trait Extractor[A] extends Function[JValue, A] { self =>
  import Extractor._

  def extract(jvalue: JValue): A 

  def validated(jvalue: JValue): Validation[Error, A] = try {
    Success(extract(jvalue))
  } catch { 
    case ex => Failure(Thrown(ex))
  }

  def map[B](f: A => B): Extractor[B] = new Extractor[B] {
    override def extract(jvalue: JValue): B = f(self.extract(jvalue))
  }

  def kleisli = Kleisli[({type λ[+α] = Validation[Error, α]})#λ, JValue, A](validated _)
  
  def apply(jvalue: JValue): A = extract(jvalue)
}

trait ValidatedExtraction[A] { this: Extractor[A] =>
  def extract(jvalue: JValue): A = validated(jvalue).fold[A](_.die, identity[A])
}

object Extractor {
  sealed trait Error {
    def die: Nothing 
    def message: String
  }

  object Error {
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

  case class Thrown(exception: Throwable) extends Error {
    def die = throw exception
    def message: String = exception.getMessage
  }

  case class Invalid(message: String) extends Error {
    def die = sys.error("JSON Extraction failure: " + message)
  }

  case class Errors(errors: NonEmptyList[Error]) extends Error {
    def die = sys.error(message)
    def message = "Multiple extraction errors occurred: " + errors.map(_.message).list.mkString(": ")
  }

  def apply[A: Manifest](f: PartialFunction[JValue, A]): Extractor[A] = new Extractor[A] with ValidatedExtraction[A] {
    override def validated(jvalue: JValue) = 
      if (f.isDefinedAt(jvalue)) Success(f(jvalue)) 
      else Failure(Invalid("Extraction not defined from value " + jvalue + " to type " + implicitly[Manifest[A]].erasure.getName))
  }
}

/** Decomposes the value into a JSON object.
 */
trait Decomposer[-A] extends Function[A, JValue] { self =>
  def decompose(tvalue: A): JValue

  def contramap[B](f: B => A): Decomposer[B] = new Decomposer[B] {
    override def decompose(b: B) = self.decompose(f(b))
  }
  
  def apply(tvalue: A): JValue = decompose(tvalue)
}

/** Serialization implicits allow a convenient syntax for serialization and 
 * deserialization when implicit decomposers and extractors are in scope.
 * <p>
 * foo.serialize
 * <p>
 * jvalue.deserialize[Foo]
 */
trait SerializationImplicits {
  case class DeserializableJValue(jvalue: JValue) {
    def deserialize[T](implicit e: Extractor[T]): T = e.extract(jvalue)
    def validated[T](implicit e: Extractor[T]): Validation[Extractor.Error, T] = e.validated(jvalue)
  }

  case class SerializableTValue[T](tvalue: T) {
    def serialize(implicit d: Decomposer[T]): JValue = d.decompose(tvalue)
  }
  
  implicit def JValueToTValue[T](jvalue: JValue): DeserializableJValue = DeserializableJValue(jvalue)
  
  implicit def TValueToJValue[T](tvalue: T): SerializableTValue[T] = SerializableTValue[T](tvalue)
}
object SerializationImplicits extends SerializationImplicits

/** Bundles default extractors, default decomposers, and serialization 
 * implicits for natural serialization of core supported types.
 */
object DefaultSerialization extends DefaultExtractors with DefaultDecomposers with SerializationImplicits 

