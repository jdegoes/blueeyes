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
    def apply(d: Double) = d.toInt
  }

  "Bijection.identity: creates Bijection which does not change data" in {
    Bijection.identity[String]("foo")         mustEqual ("foo")
    Bijection.identity[String].unapply("foo") mustEqual ("foo")
  }
  "Bijection.inverse: creates inverse Bijection" in {
    val inversed = ByteArrayToString.inverse
    inversed.unapply(Array[Byte]('f', 'o', 'o'))  mustEqual("foo")
    inversed("foo").toList                        mustEqual(List[Byte]('f', 'o', 'o'))
  }
  "Bijection.compose: creates composed Bijection" in {
    val composed = atoi.andThen(itof)
    composed("1.23") must_== 1.23d
  }
}
