package blueeyes.core.data

import DefaultBijections._
import org.specs2.mutable.Specification

class BijectionSpec extends Specification {
  object atoi extends Bijection[String, Int] {
    def apply(s: String) = s.toInt
    def unapply(i: Int) = i.toString
  }

  object itof extends Bijection[Int, Double] {
    def apply(i: Int) = i.toDouble
    def unapply(d: Double) = d.toInt
  }

  "Bijection.identity: creates Bijection which does not change data" in {
    Bijection.identity[String]("foo")         mustEqual ("foo")
    Bijection.identity[String].unapply("foo") mustEqual ("foo")
  }
  "Bijection.inverse: creates inverse Bijection" in {
    val b = ByteArrayToString
    b(Array[Byte]('f', 'o', 'o')) must_== "foo"
    b.inverse.unapply(Array[Byte]('f','o','o')) must_== "foo"
    b.inverse("foo").toList must_== List[Byte]('f', 'o', 'o')
  }
  "Bijection.compose: creates composed Bijection" in {
    val composed = atoi.andThen(itof)
    composed("2") must_== 2.0d
  }
}
