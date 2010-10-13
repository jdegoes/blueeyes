package blueeyes.json.xschema {

import blueeyes.json.JsonAST._

/** Extracts the value from a JSON object.
 */
trait Extractor[T] extends Function[JValue, T] {
  def extract(jvalue: JValue): T
  
  def apply(jvalue: JValue): T = extract(jvalue)
}

/** Decomposes the value into a JSON object.
 */
trait Decomposer[T] extends Function[T, JValue] {
  def decompose(tvalue: T): JValue
  
  def apply(tvalue: T): JValue = decompose(tvalue)
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
object DefaultSerialization extends DefaultExtractors with DefaultDecomposers with SerializationImplicits {
}

}