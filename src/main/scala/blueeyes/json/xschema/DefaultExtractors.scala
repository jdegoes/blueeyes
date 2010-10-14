package blueeyes.json.xschema {

import blueeyes.json.JsonAST._
import java.util.{Date => JDate}

/** Extractors for all basic types.
 */
trait DefaultExtractors {
  implicit val JValueExtractor: Extractor[JValue] = new Extractor[JValue] {
    def extract(jvalue: JValue): JValue = jvalue
  }
  
  implicit val StringExtractor: Extractor[String] = new Extractor[String] {
    def extract(jvalue: JValue): String = jvalue match {
      case JString(str) => str
      
      case JInt(i) => i.toString
      case JDouble(d) => d.toString
      case JBool(b) => b.toString
      
      case _ => error("Expected String but found: " + jvalue)
    }
  }
  
  implicit val BooleanExtractor: Extractor[Boolean] = new Extractor[Boolean] {
    def extract(jvalue: JValue): Boolean = jvalue match {
      case JBool(b) => b
      
      case JString(s) if (s.toLowerCase == "true")  => true
      case JString(s) if (s.toLowerCase == "false") => false
      
      case JString(s) if (s.toLowerCase == "1") => true
      case JString(s) if (s.toLowerCase == "0") => false
      
      case JInt(i) if (i.intValue == 1) => true
      case JInt(i) if (i.intValue == 0) => false
      
      case _ => error("Expected Boolean but found: " + jvalue)
    }
  }
  
  implicit val IntExtractor: Extractor[Int] = new Extractor[Int] {
    def extract(jvalue: JValue): Int = jvalue match {
      case JInt(i)    => i.intValue
      case JDouble(d) => d.toInt
      
      case JString(s) => s.toInt
      
      case _ => error("Expected Integer but found: " + jvalue)
    }
  }
  
  implicit val LongExtractor: Extractor[Long] = new Extractor[Long] {
    def extract(jvalue: JValue): Long = jvalue match {
      case JInt(i)    => i.longValue
      case JDouble(d) => d.toLong
      
      case JString(s) => s.toLong
      
      case _ => error("Expected Long but found: " + jvalue)
    }
  }
  
  implicit val FloatExtractor: Extractor[Float] = new Extractor[Float] {
    def extract(jvalue: JValue): Float = jvalue match {
      case JInt(i)    => i.floatValue
      case JDouble(d) => d.toFloat
      
      case JString(s) => s.toFloat
      
      case _ => error("Expected Float but found: " + jvalue)
    }
  }

  implicit val DoubleExtractor: Extractor[Double] = new Extractor[Double] {
    def extract(jvalue: JValue): Double = jvalue match {
      case JInt(i)    => i.doubleValue
      case JDouble(d) => d
      
      case JString(s) => s.toDouble

      case _ => error("Expected Double but found: " + jvalue)
    }
  }
  
  implicit val DateExtractor: Extractor[JDate] = new Extractor[JDate] {
    def extract(jvalue: JValue): JDate = new JDate(LongExtractor.extract(jvalue))
  }
  
  implicit def OptionExtractor[T](implicit extractor: Extractor[T]): Extractor[Option[T]] = new Extractor[Option[T]] {
    def extract(jvalue: JValue): Option[T] = jvalue match {
      case JNothing | JNull => None
      case x: JValue => Some(extractor.extract(x))
    }
  }
  
  implicit def Tuple2Extractor[T1, T2](implicit extractor1: Extractor[T1], extractor2: Extractor[T2]): Extractor[(T1, T2)] = new Extractor[(T1, T2)] {
    def extract(jvalue: JValue): (T1, T2) = jvalue match {
      case JArray(values) if (values.length == 2) => (extractor1(values(0)), extractor2(values(1)))

      case _ => error("Expected Array of length 2 but found: " + jvalue)
    }
  }
  
  implicit def Tuple3Extractor[T1, T2, T3](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3]): Extractor[(T1, T2, T3)] = new Extractor[(T1, T2, T3)] {
    def extract(jvalue: JValue): (T1, T2, T3) = jvalue match {
      case JArray(values) if (values.length == 3) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)))

      case _ => error("Expected Array of length 3 but found: " + jvalue)
    }
  }
  
  implicit def Tuple4Extractor[T1, T2, T3, T4](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3], extractor4: Extractor[T4]): Extractor[(T1, T2, T3, T4)] = new Extractor[(T1, T2, T3, T4)] {
    def extract(jvalue: JValue): (T1, T2, T3, T4) = jvalue match {
      case JArray(values) if (values.length == 4) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)), extractor4(values(3)))

      case _ => error("Expected Array of length 4 but found: " + jvalue)
    }
  }
  
  implicit def Tuple5Extractor[T1, T2, T3, T4, T5](implicit extractor1: Extractor[T1], extractor2: Extractor[T2], extractor3: Extractor[T3], extractor4: Extractor[T4], extractor5: Extractor[T5]): Extractor[(T1, T2, T3, T4, T5)] = new Extractor[(T1, T2, T3, T4, T5)] {
    def extract(jvalue: JValue): (T1, T2, T3, T4, T5) = jvalue match {
      case JArray(values) if (values.length == 5) => (extractor1(values(0)), extractor2(values(1)), extractor3(values(2)), extractor4(values(3)), extractor5(values(4)))

      case _ => error("Expected Array of length 5 but found: " + jvalue)
    }
  }
  
  implicit def ArrayExtractor[T](implicit m: ClassManifest[T], elementExtractor: Extractor[T]): Extractor[Array[T]] = new Extractor[Array[T]] {
    def extract(jvalue: JValue): Array[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _).toArray

      case _ => error("Expected Array but found: " + jvalue)
    }
  }
  
  implicit def SetExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[Set[T]] = new Extractor[Set[T]] {
    def extract(jvalue: JValue): Set[T] = jvalue match {
      case JArray(values) => Set(values.map(elementExtractor.extract _): _*)

      case _ => error("Expected Set but found: " + jvalue)
    }
  }
  
  implicit def ListExtractor[T](implicit elementExtractor: Extractor[T]): Extractor[List[T]] = new Extractor[List[T]] {
    def extract(jvalue: JValue): List[T] = jvalue match {
      case JArray(values) => values.map(elementExtractor.extract _)

      case _ => error("Expected List but found: " + jvalue)
    }
  }
  
  implicit def MapExtractor[K, V](implicit keyExtractor: Extractor[K], valueExtractor: Extractor[V]): Extractor[Map[K, V]] = new Extractor[Map[K, V]] {
    def extract(jvalue: JValue): Map[K, V] = Map(ListExtractor(Tuple2Extractor(keyExtractor, valueExtractor)).extract(jvalue): _*)
  }
  
  implicit def StringMapExtractor[V](implicit valueExtractor: Extractor[V]): Extractor[Map[String, V]] = new Extractor[Map[String, V]] {
    def extract(jvalue: JValue): Map[String, V] = jvalue match {
      case JObject(fields) => Map((fields.map { field => (field.name, valueExtractor.extract(field.value)) }): _*)
      
      case _ => Map(ListExtractor(Tuple2Extractor(StringExtractor, valueExtractor)).extract(jvalue): _*)
    }
  }
}
object DefaultExtractors extends DefaultExtractors

}