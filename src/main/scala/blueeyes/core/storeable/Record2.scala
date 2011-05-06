package blueeyes.core.storeable

sealed trait Record2[T1, T2] extends Product with Storeable{
  def companion[T <: Record2Companion[_ <: Record2[T1, T2], T1, T2]]
}

trait Record2Companion[R <: Record2[T1, T2], T1, T2] extends Product2[Field[R, T1], Field[R, T2]] with Companion[R]


import scalaz.{Success, Failure}
case class Person(name: String, age: Int) extends Record2[String, Int] {
  def companion[Person] = Person
}

object Person extends Record2Companion[Person, String, Int]{
  val _1 = Field("name", {v: Person => v.name}, {(v: Person, name: String) => v.copy(name = name)}, Failure("No name specified!"))

  val _2 = Field("age", {v: Person => v.age}, {(v: Person, age: Int) => v.copy(age = age)}, Success(-1))

  def example: Person = Person("John Doe", 123)
}