package blueeyes.json.serialization

import blueeyes.json._

import scalaz.{Validation, Success, Failure, NonEmptyList, Kleisli, Plus, Functor}
import scalaz.syntax.bifunctor._
import Validation._
import NonEmptyList._

/** Decomposes the value into a JSON object.
 */
trait Decomposer[A] { self =>
  def decompose(tvalue: A): JValue

  def contramap[B](f: B => A): Decomposer[B] = new Decomposer[B] {
    override def decompose(b: B) = self.decompose(f(b))
  }
  
  def apply(tvalue: A): JValue = decompose(tvalue)

  def unproject(jpath: JPath): Decomposer[A] = new Decomposer[A] {
    override def decompose(b: A) = JUndefined.unsafeInsert(jpath, self.decompose(b))
  }
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
    def validated[T](jpath: JPath)(implicit e: Extractor[T]) = e.validated(jvalue, jpath)
  }

  case class SerializableTValue[T](tvalue: T) {
    def serialize(implicit d: Decomposer[T]): JValue = d.decompose(tvalue)
    def jv(implicit d: Decomposer[T]): JValue = d.decompose(tvalue)
  }
  
  implicit def JValueToTValue[T](jvalue: JValue): DeserializableJValue = DeserializableJValue(jvalue)
  
  implicit def TValueToJValue[T](tvalue: T): SerializableTValue[T] = SerializableTValue[T](tvalue)
}

object SerializationImplicits extends SerializationImplicits

/** Bundles default extractors, default decomposers, and serialization 
 * implicits for natural serialization of core supported types.
 */
object DefaultSerialization extends DefaultExtractors with DefaultDecomposers with SerializationImplicits 


// vim: set ts=4 sw=4 et:
