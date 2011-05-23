package blueeyes.core.http

import org.scalacheck._
import org.scalacheck.Prop.forAllNoShrink
import scala.util.Random
import blueeyes.util.SpecialCharToStringTranscoder
import org.specs.{ScalaCheck, Specification}

class URITranscodersSpec extends Specification with ScalaCheck{
  private val random = new Random()
  private def unicodeChar = Gen((p: Gen.Params) => {
    var c = 0
    do {
      c = random.nextInt(1000)
    } while (!Character.isDefined(c))
    Some(c.toChar)
  })

  private def pathElementGen: Gen[String] = {
    for {
      size   <- Gen.choose(1, 10)
      values <- Gen.listOfN(size, Gen.oneOf(Gen.oneOf[Char](URITranscoders.SafePathChars.toCharArray().toList), unicodeChar, Gen.alphaChar)).map(v => "/" + v.mkString(""))
    } yield {values}
  }

  private def queryElementGen: Gen[String] = {
    def gen = Gen.oneOf(Gen.oneOf[Char](URITranscoders.SafeQueryChars.toCharArray().toList), unicodeChar, Gen.alphaChar)
    for {
      keySize   <- Gen.choose(1, 10)
      valueSize <- Gen.choose(1, 10)
      key   <- Gen.listOfN(keySize, gen).map(v => v.mkString(""))
      value <- Gen.listOfN(valueSize, gen).map(v => v.mkString(""))
    } yield {key + "=" + value}
  }
  private def pathGen: Gen[String] = {
    for {
      size   <- Gen.choose(1, 10)
      values <- Gen.listOfN(size, pathElementGen)
    } yield {values.mkString("")}
  }
  private def queryGen: Gen[String] = {
    for {
      size   <- Gen.choose(1, 10)
      values <- Gen.listOfN(size, queryElementGen).map(v => v.mkString("&"))
    } yield {values.mkString("")}
  }

  "URITranscoders.pathTranscoder" should{
    "encode and decode path" in{
      passTest(pathGen, URITranscoders.pathTranscoder)
    }
  }
  "URITranscoders.queryTranscoder" should{
    "encode and decode query" in{
      passTest(queryGen, URITranscoders.queryTranscoder)
    }
  }
  private def passTest(gen: Gen[String], trascoder: SpecialCharToStringTranscoder) = forAllNoShrink(gen){n: String => trascoder.decode(trascoder.encode(n)) == n} must pass
}