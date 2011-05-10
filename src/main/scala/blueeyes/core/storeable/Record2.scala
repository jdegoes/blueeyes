package blueeyes.core.storeable

trait Record2[T1, T2] extends Product with Record{ self =>
  def companion: Record2Companion[_ <: Record2[T1, T2], T1, T2]
}

trait Record2Companion[R <: Record2[T1, T2], T1, T2] extends Product2[Field[R, T1], Field[R, T2]] with Companion[R] with ValueImplicits



//
//import scalaz.{Failure, Success}
//case class Person(name: String, age: Int, orders: List[Person]) extends Record3[String, Int, List[Person]] {
//  def companion = Person
//}
//
//object Person extends Record3Companion[Person, String, Int, List[Person]]{
//  val _1 = Field[Person, String]("name", _.name, (person, name) => person.copy(name = name), Failure("No name specified!"))
//
//  val _2 = Field[Person, Int]("age", _.age, (person, age) => person.copy(age = age), -1)
//
//  val _3 = Field[Person, List[Person]]("orders", _.orders, (person, orders) => person.copy(orders = orders), Failure("No name specified!"))
//
//  def _example: Person = Person("John Doe", 123, Nil)
//}