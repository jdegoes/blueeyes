package blueeyes.json

trait Json[A] {
 def fold[Z](value: A, prim: A => Z, arr: Iterable[A] => Z, obj: Iterable[(String, A)] => Z): Z

 def toArray(i: Iterable[A]): A

 def toObject(i: Iterable[(String, A)]): A
}

class JsonLike[A](value: Option[A])(implicit json: Json[A]) {
//  def \ (name: String): Option[A] = json.fold(None, None, None, obj => obj.find(_ == name))
}

object JsonLike {
 implicit def ValueToJsonLike[A: Json](value: A):          JsonLike[A] = new JsonLike[A](Some(value))
 implicit def OptionToJsonLike[A: Json](value: Option[A]): JsonLike[A] = new JsonLike[A](value)
}