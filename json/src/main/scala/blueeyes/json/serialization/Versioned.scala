package blueeyes.json
package serialization

import IsoSerialization._
import Extractor._
import DefaultSerialization._

import shapeless._
import scalaz._
import scalaz.std.option._
import scalaz.syntax.show._
import scalaz.syntax.order._
import scalaz.syntax.semigroup._
import scalaz.Validation._

case class Version(major: Int, minor: Int, micro: Option[Int] = None, classifier: Option[String] = None) {
  def isBackwardCompatible(other: Version) = this >= other && this.major == other.major
}

object SVersion {
  val VPattern = """(\d+)\.(\d+)(?:\.(\d+))?(?:-(.*))?""".r

  def unapply(s: String): Option[Version] = s match {
    case VPattern(major, minor, micro, classifier) => Some(Version(major.toInt, minor.toInt, Option(micro).map(_.toInt), Option(classifier)))
    case _ => None
  }
}

object Version {
  implicit val tc: Order[Version] with Show[Version] = new Order[Version] with Show[Version] {
    import scalaz.Ordering
    import scalaz.Ordering._

    def order(v1: Version, v2: Version): Ordering = {
      import scalaz.syntax.apply._
      (v1.major ?|? v2.major) |+| 
      (v1.minor ?|? v2.minor) |+|
      (^(v1.micro, v2.micro) { _ ?|? _ } getOrElse EQ)
    }

    override def shows(v: Version): String = {
      val microS = v.micro.map("." + _).getOrElse("")
      val clS = v.classifier.map("-" + _).getOrElse("")
      v.major + "." + v.minor + microS + clS
    }
  }

  implicit val serialization: Extractor[Version] with Decomposer[Version] = new Extractor[Version] with Decomposer[Version] {
    def decompose(v: Version) = JString(v.shows)

    def validated(jvalue: JValue) = jvalue match {
      case JString(SVersion(v)) => success(v)
      case _ => Failure(Invalid("Version " + jvalue.renderCompact + " + did not match the expected pattern."))
    }
  }
}

object Versioned {
  val defaultVersionProperty = JPath(".schemaVersion")

  def extractorV[T] = new MkExtractorV[T]
  def decomposerV[T] = new MkDecomposerV[T]
  def serializationV[T] = new MkSerializationV[T]

  implicit def toToVersion(s: String): ToVersion = new ToVersion(s)
  class ToVersion(s: String) {
    def v: Version = (s: @unchecked) match { case SVersion(v) => v }
  }

  def versioned[A](extractor: Extractor[A], version: Option[Version], versionField: JPath): Extractor[A] = new Extractor[A] {
    def validated(jv: JValue) = {
      import scalaz.syntax.traverse._

      version.traverse[({ type λ[α] = Validation[Error, α] })#λ, Version] { v => 
        jv.validated[Option[Version]]("schemaVersion") flatMap {
          case Some(vrec) => 
            if (v.isBackwardCompatible(vrec)) success(vrec) 
            else failure(Invalid("schemaVersion value " + vrec.shows + " was incompatible with desired version " + v.shows))
          case None => 
            failure(Invalid("schemaVersion property missing for value " + jv.renderCompact + "; was expecting " + v.shows))
        } 
      } flatMap { _: Option[Version] => 
        extractor.validated(jv)
      }
    }
  }

  def versioned[A](decomposer: Decomposer[A], version: Option[Version], versionField: JPath): Decomposer[A] = new Decomposer[A] {
    def decompose(a: A): JValue = {
      val baseResult = decomposer.decompose(a)
      version map { v =>
       if (baseResult.isInstanceOf[JObject]) {
          baseResult.unsafeInsert(versionField, v.jv)
        } else {
          sys.error("Cannot version primitive or array values!")
        }
      } getOrElse {
        baseResult
      }
    }
  }

  class MkDecomposerV[T] {
    def apply[F <: HList, L <: HList](fields: F, version: Option[Version])(implicit iso: Iso[T, L], decomposer: DecomposerAux[F, L]): Decomposer[T] =
      versioned(new IsoDecomposer(fields, iso, decomposer), version, defaultVersionProperty)
  }

  class MkExtractorV[T] {
    def apply[F <: HList, L <: HList](fields: F, version: Option[Version])(implicit iso: Iso[T, L], extractor: ExtractorAux[F, L]): Extractor[T] =
      versioned(new IsoExtractor(fields, iso, extractor), version, defaultVersionProperty)
  }

  class MkSerializationV[T] {
    def apply[F <: HList, L <: HList](fields: F, version: Option[Version])
      (implicit iso: Iso[T, L], decomposer: DecomposerAux[F, L], extractor: ExtractorAux[F, L]): (Decomposer[T], Extractor[T]) =
        (versioned(new IsoDecomposer(fields, iso, decomposer), version, defaultVersionProperty),
         versioned(new IsoExtractor(fields, iso, extractor), version, defaultVersionProperty))
  }
}
