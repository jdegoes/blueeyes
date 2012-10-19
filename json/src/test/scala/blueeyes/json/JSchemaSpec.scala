package blueeyes.json

import org.scalacheck._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scalaz.Ordering._

object JSchemaSpec extends Specification with ScalaCheck with ArbitraryJPath with ArbitraryJValue {
  override val defaultPrettyParams = Pretty.Params(2)

  def f(v: JValue): JSchema = JSchema.fixed(v)

  "JSchema.JSON" should {
    "validate all JSON without exploding" in {
      check { (value: JValue) =>
        JSchema.JSON.validate(value)
      }
    }
  }

  "JSchema.fixed" should {
    "validate value that created the schema" in {
      check { (value: JValue) =>
        JSchema.fixed(value).validate(value)
      }
    }

    "validate only the value that created the schema" in {
      check { (tuple: (JValue, JValue)) =>
        val (v1, v2) = tuple

        (v1 == v2) | !JSchema.fixed(v1).validate(v2)
      }
    }

    "not break regression" in {
      val v1 = JParser.parse("""{"yitAwQemwwsadhpeGj105763":"wfetLyqmpjrkksvnekdqetmx","zykzjslLdcmtHmusqnwwzft202253":-4.611686018427387904E-2147483609,"cyqk382845":1E+1983360066}""")

      val v2 = JObject.empty

      JSchema.fixed(v1).validate(v2) must beFalse
    }
  }

  "JSchema.minimize" should {
    "when bounded" >> {
      "convert homogeneous fixed to singleton unfixed" in {
        val fixed = f(JNum(1)) | f(JNum(2)) | f(JNum(3)) | f(JNum(4)) | f(JNum(5)) | f(JNum(6))

        fixed.minimize(5) mustEqual JNumSchema
      }

      "convert heterogeneous fixed to heterogeneous unfixed" in {
        val fixed = f(JNum(1)) | f(JNum(2)) | f(JNum(3)) | f(JString("foo")) | f(JNum(5)) | f(JNum(6))

        fixed.minimize(5) mustEqual (JNumSchema | JStringSchema)
      }
    }
  }

  "JSchema.unfixed" should {
    "validate value that created the schema" in {
      check { (value: JValue) =>
        JSchema.unfixed(value).validate(value)
      }
    }

    "validate another value with the same schema" in {
      check { (v1: JValue) =>
        val v2 = v1.mapDown { 
          case JNum(_) => JNum(1L)
          case JString(_) => JString("foo blah blah")
          case JBool(v) => JBool(!v)
          case x => x
        }

        JSchema.unfixed(v1).validate(v2)
      }
    }
  }
}