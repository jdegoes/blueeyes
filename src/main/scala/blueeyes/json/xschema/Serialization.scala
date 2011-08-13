package blueeyes.json.xschema

import blueeyes.json.JsonAST._

/** Extracts the value from a JSON object.
 */
trait Extractor[A] extends Function[JValue, A] { self =>
  def extract(jvalue: JValue): A

  def map[B](f: A => B): Extractor[B] = new Extractor[B] {
    override def extract(jvalue: JValue): B = f(self.extract(jvalue))
  }
  
  def apply(jvalue: JValue): A = extract(jvalue)
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
    def deserialize[T](implicit e: Extractor[T]): T = e(jvalue)
  }
  case class SerializableTValue[T](tvalue: T) {
    def serialize(implicit d: Decomposer[T]): JValue = d(tvalue)
  }
  
  implicit def JValueToTValue[T](jvalue: JValue): DeserializableJValue = DeserializableJValue(jvalue)
  
  implicit def TValueToJValue[T](tvalue: T): SerializableTValue[T] = SerializableTValue[T](tvalue)
}
object SerializationImplicits extends SerializationImplicits

/** Bundles default extractors, default decomposers, and serialization 
 * implicits for natural serialization of core supported types.
 */
object DefaultSerialization extends DefaultExtractors with DefaultDecomposers with SerializationImplicits 

