package blueeyes.json
package serialization

import DefaultSerialization._
import IsoSerialization._
import Extractor._

import shapeless._
import scalaz._

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck._

import Prop.forAll
import Arbitrary._


object VersionedSpec extends Specification with ScalaCheck with ArbitraryJPath with ArbitraryJValue {
  import IsoSerializationSpec._
  import Versioned._

  "versioned serialization" should {
    "serialize a simple case class" in {
      val fooDecomp = decomposerV[Foo](fooSchema, Some("1.0".v))
      
      val result = fooDecomp.decompose(foo)
      
      result must_== JParser.parseUnsafe("""{ "s": "Hello world", "i": 23, "b": true, "schemaVersion": "1.0" }""")
    }
  }

  "versioned deserialization" should {
    "extract to a simple case class" in {
      val fooExtract = extractorV[Foo](fooSchema, Some("1.0".v))
      
      val result = fooExtract.extract(
        jobject(
          jfield("s", "Hello world"),
          jfield("i", 23),
          jfield("b", true),
          jfield("schemaVersion", "1.0")
        )
      )
      
      result must_== foo
    }

    "refuse to deserialize an object missing a version" in {
      val fooExtract = extractorV[Foo](fooSchema, Some("1.0".v))
      
      val result = fooExtract.validated(
        jobject(
          jfield("s", "Hello world"),
          jfield("i", 23),
          jfield("b", true)
        )
      )
      
      result must beLike {
        case Failure(Invalid(message, None)) => message must startWith(".schemaVersion property missing")
      }
    }

    "refuse to deserialize an object from a future version" in {
      val fooExtract = extractorV[Foo](fooSchema, Some("1.0".v))
      
      val result = fooExtract.validated(
        jobject(
          jfield("s", "Hello world"),
          jfield("i", 23),
          jfield("b", true),
          jfield("schemaVersion", "1.1")
        )
      )
      
      result must beLike {
        case Failure(Invalid(message, None)) => message must contain("was incompatible with desired version")
      }
    }

    "deserialize an object from a major-compatible prior version" in {
      val fooExtract = extractorV[Foo](fooSchema, Some("1.1".v))
      
      val result = fooExtract.validated(
        jobject(
          jfield("s", "Hello world"),
          jfield("i", 23),
          jfield("b", true),
          jfield("schemaVersion", "1.0")
        )
      )
      
      result must beLike {
        case Success(v) => v must_== foo
      }
    }
  }
}

