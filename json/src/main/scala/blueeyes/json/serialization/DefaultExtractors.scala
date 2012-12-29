package blueeyes.json.serialization 

import blueeyes.json._
import Extractor._

import java.util.{Date => JDate}
import scala.math.BigDecimal
import org.joda.time.{DateTime, DateTimeZone}

import scalaz._
import scalaz.Validation._
import scalaz.std.list._
import scalaz.std.function._
import scalaz.std.map._
import scalaz.std.tuple._
import scalaz.syntax.arrow._
import scalaz.syntax.apply._
import scalaz.syntax.bifunctor._
import scalaz.syntax.traverse.ToTraverseOps
import scalaz.syntax.id._

/** Extractors for all basic types.
 */
trait DefaultExtractors {
  private type VE[a] = Validation[Error, a]

  implicit val JValueExtractor: Extractor[JValue] = new Extractor[JValue] {
    override def extract(jvalue: JValue): JValue = jvalue
    def validated(jvalue: JValue) = Success(jvalue)
  }
  
  implicit val StringExtractor: Extractor[String] = new Extractor[String] {
    def validated(jvalue: JValue) = jvalue match {
      case JString(str) => Success(str)
      case JNum(i) => Success(i.toString)
      case JBool(b) => Success(b.toString)
      case _ => invalidv("Expected String but found: " + jvalue)
    }
  }
  
  implicit val BooleanExtractor: Extractor[Boolean] = new Extractor[Boolean] {
    def validated(jvalue: JValue) = jvalue match {
      case JBool(b) => Success(b)
      case JString(s) if (s.toLowerCase == "true")  => Success(true)
      case JString(s) if (s.toLowerCase == "false") => Success(false)
      case _ => invalidv("Expected Boolean but found: " + jvalue)
    }
  }
  
  implicit val IntExtractor: Extractor[Int] = new Extractor[Int] {
    def validated(jvalue: JValue) = jvalue match {
      case JNum(i)    => tryv(i.toIntExact)
      case JString(s) => tryv(s.toInt)
      case _ => invalidv("Expected Integer but found: " + jvalue)
    }
  }
  
  implicit val LongExtractor: Extractor[Long] = new Extractor[Long] {
    def validated(jvalue: JValue) = jvalue match {
      case JNum(i)    => tryv(i.toLongExact)
      case JString(s) => tryv(s.toLong)
      case _ => invalidv("Expected Long but found: " + jvalue)
    }
  }
  
  implicit val FloatExtractor: Extractor[Float] = new Extractor[Float] {
    def validated(jvalue: JValue) = jvalue match {
      case JNum(i)    => Success(i.toFloat)
      case JString(s) => tryv(s.toFloat)
      case _ => invalidv("Expected Float but found: " + jvalue)
    }
  }

  implicit val DoubleExtractor: Extractor[Double] = new Extractor[Double] {
    def validated(jvalue: JValue) = jvalue match {
      case JNum(i)    => Success(i.toDouble)
      case JString(s) => tryv(s.toDouble)
      case _ => invalidv("Expected Double but found: " + jvalue)
    }
  }
  
  implicit val BigDecimalExtractor: Extractor[BigDecimal] = new Extractor[BigDecimal] {
    def validated(jvalue: JValue) = jvalue match {
      case JNum(i)    => Success(i)
      case JString(s) => tryv(BigDecimal(s))
      case _ => invalidv("Expected BigDecimal but found: " + jvalue)
    }
  }

  implicit val DateExtractor: Extractor[JDate] = LongExtractor map { new JDate(_) }
  
  implicit def OptionExtractor[T](implicit extractor: Extractor[T]): Extractor[Option[T]] = new Extractor[Option[T]] {
    def validated(jvalue: JValue) = jvalue match {
      case JUndefined | JNull => Success(None)
      case x => extractor.validated(x) map { Some(_) }
    }
  }
  
  implicit def Tuple2Extractor[A, B](implicit ea: Extractor[A], eb: Extractor[B]): Extractor[(A, B)] = new Extractor[(A, B)] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) if (values.length == 2) => 
        ea.validated(values(0)) tuple eb.validated(values(1))

      case _ => invalidv("Expected Array of length 2 but found: " + jvalue)
    }
  }
  
  implicit def Tuple3Extractor[A, B, C](implicit ea: Extractor[A], eb: Extractor[B], ec: Extractor[C]): Extractor[(A, B, C)] = new Extractor[(A, B, C)] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) if (values.length == 3) => 
        ^[VE, A, B, C, (A, B, C)](ea.validated(values(0)), eb.validated(values(1)), ec.validated(values(2))) {
          Tuple3.apply _
        }

      case _ => invalidv("Expected Array of length 3 but found: " + jvalue)
    }
  }
  
  implicit def Tuple4Extractor[A, B, C, D](implicit ea: Extractor[A], eb: Extractor[B], ec: Extractor[C], ed: Extractor[D]): Extractor[(A, B, C, D)] = new Extractor[(A, B, C, D)] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) if (values.length == 4) => 
        ^[VE, A, B, C, D, (A, B, C, D)](ea.validated(values(0)), eb.validated(values(1)), ec.validated(values(2)), ed.validated(values(3))) { Tuple4.apply _ }

      case _ => invalidv("Expected Array of length 4 but found: " + jvalue)
    }
  }
  
  implicit def Tuple5Extractor[A, B, C, D, E](implicit ea: Extractor[A], eb: Extractor[B], ec: Extractor[C], ed: Extractor[D], ee: Extractor[E]): Extractor[(A, B, C, D, E)] = new Extractor[(A, B, C, D, E)] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) if (values.length == 5) => 
        ^[VE, A, B, C, D, E, (A, B, C, D, E)](ea.validated(values(0)), eb.validated(values(1)), ec.validated(values(2)), ed.validated(values(3)), ee.validated(values(4))) {
          Tuple5.apply _
        }

      case _ => invalidv("Expected Array of length 5 but found: " + jvalue)
    }
  }
  
  implicit def ArrayExtractor[T](implicit m: ClassManifest[T], elementExtractor: Extractor[T]): Extractor[Array[T]] = new Extractor[Array[T]] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) => values.map(elementExtractor.validated _).sequence[VE, T].map(_.toArray)
      case _ => invalidv("Expected JArray but found: " + jvalue)
    }
  }
  
  implicit def SetExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[Set[T]] = new Extractor[Set[T]] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) => values.map(elementExtractor.validated _).sequence[VE, T].map(_.toSet)
      case _ => invalidv("Expected JArray but found: " + jvalue)
    }
  }
  
  implicit def ListExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[List[T]] = new Extractor[List[T]] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) => values.map(elementExtractor.validated _).sequence[VE, T]
      case _ => invalidv("Expected JArray but found: " + jvalue)
    }
  }
  
  implicit def VectorExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[Vector[T]] = new Extractor[Vector[T]] {
    def validated(jvalue: JValue) = jvalue match {
      case JArray(values) => values.map(elementExtractor.validated _).sequence[VE, T] map { vs => Vector(vs: _*) }
      case _ => invalidv("Expected JArray but found: " + jvalue)
    }
  }
  
  implicit def MapExtractor[K, V](implicit keyExtractor: Extractor[K], valueExtractor: Extractor[V]): Extractor[Map[K, V]] = new Extractor[Map[K, V]] {
    def validated(jvalue: JValue) = {
      ListExtractor(Tuple2Extractor(keyExtractor, valueExtractor)).validated(jvalue).map(_.toMap)
    }
  }
  
  implicit def StringMapExtractor[V](implicit valueExtractor: Extractor[V]): Extractor[Map[String, V]] = new Extractor[Map[String, V]] {
    private val ev = (valueExtractor.validated _).second[String]

    def validated(jvalue: JValue) = jvalue match {
      case JObject(fields) => 
        Traverse[({ type l[a] = Map[String, a] })#l].sequence[VE, V] {
          fields.mapValues(valueExtractor.validated)
        }

      case other => 
        ListExtractor(Tuple2Extractor(StringExtractor, valueExtractor)).validated(other).map(_.toMap)
    }
  }

  implicit val DateTimeExtractor = new Extractor[DateTime] {
    def validated(jvalue: JValue) = {
      LongExtractor.validated(jvalue) map { millis => new DateTime(millis, DateTimeZone.UTC) }
    }
  }
}

object DefaultExtractors extends DefaultExtractors
