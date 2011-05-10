package blueeyes.core.storeable

trait Record3[T1, T2, T3] extends Product with Record[Record3Companion[_, _, T1, T2, T3]]

trait Record3Companion[P <: Record[_], R <: Record3[T1, T2, T3], T1, T2, T3] extends Product3[Field[R, T1], Field[R, T2], Field[R, T3]] with Companion[P, R]

//import scalaz.{Failure, Success}
//case class Person(name: String, age: Int, orders: List[Person]) extends Record3[String, Int, List[Person]] {
//  def companion = Person
//}
//
//object Person extends Record3Companion[RecordNothing.type, Person, String, Int, List[Person]]{
//  val _1 = Field[Person, String]("name", _.name, (person, name) => person.copy(name = name), Failure("No name specified!"))
//
//  val _2 = Field[Person, Int]("age", _.age, (person, age) => person.copy(age = age), -1)
//
//  val _3 = Field[Person, List[Person]]("orders", _.orders, (person, orders) => person.copy(orders = orders), Failure("No name specified!"))
//
//  def _example: Person = Person("John Doe", 123, Nil)
//}