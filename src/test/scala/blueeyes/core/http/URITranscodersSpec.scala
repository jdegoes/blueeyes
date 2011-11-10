package blueeyes.core.http

import org.scalacheck._
import org.scalacheck.Prop.forAllNoShrink
import scala.util.Random
import blueeyes.util.SpecialCharToStringTranscoder
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.specs2.matcher.MustThrownMatchers

class URITranscodersSpec extends Specification with ScalaCheck  with MustThrownMatchers{
  private val random = new Random()

  private def unicodeChar = Gen((p: Gen.Params) => {
    var c = 0
    do {
      c = random.nextInt(1000)
    } while (!Character.isDefined(c))
    Some(c.toChar)
  })

  private def pathElementGen: Gen[String] = Gen.listOfN(listSize, Gen.oneOf(Gen.oneOf[Char](URITranscoders.SafePathChars.toCharArray().toList), unicodeChar, Gen.alphaChar)).map(v => "/" + v.mkString(""))

  private def queryElementGen: Gen[String] = {
    def gen = Gen.oneOf(Gen.oneOf[Char](URITranscoders.SafeQueryChars.toCharArray().toList), unicodeChar, Gen.alphaChar)
    for {
      key   <- Gen.listOfN(listSize, gen).map(v => v.mkString(""))
      value <- Gen.listOfN(listSize, gen).map(v => v.mkString(""))
    } yield {key + "=" + value}
  }

  private def pathGen: Gen[String]  = for (values <- Gen.listOfN(listSize, pathElementGen)) yield values.mkString("")

  private def queryGen: Gen[String] = for (values <- Gen.listOfN(listSize, queryElementGen).map(v => v.mkString("&"))) yield values.mkString("")

  private def listSize = Gen.choose(1, 10).sample.get

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
  private def passTest(gen: Gen[String], trascoder: SpecialCharToStringTranscoder) = forAllNoShrink(gen){n: String => trascoder.decode(trascoder.encode(n)) must be_==(n)}
}