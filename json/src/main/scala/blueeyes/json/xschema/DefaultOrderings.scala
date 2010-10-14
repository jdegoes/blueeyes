package blueeyes.json.xschema {

import blueeyes.json.JsonAST._
import java.util.{Date => JDate}

trait DefaultOrderings {
  private implicit def AnyWithOrderingToOrderedAny[T](t1: T)(implicit ord: Ordering[T]): Ordered[T] = new Ordered[T] { def compare(t2: T) = ord.compare(t1, t2); }
  
  val StringOrdering: Ordering[String] = new Ordering[String] {
    def compare(v1: String, v2: String): Int = v1.compare(v2)
  }
  
  val BooleanOrdering: Ordering[Boolean] = new Ordering[Boolean] {
    def compare(v1: Boolean, v2: Boolean): Int = v1.compare(v2)
  }
  
  val IntOrdering: Ordering[Int] = new Ordering[Int] {
    def compare(v1: Int, v2: Int): Int = v1.compare(v2)
  }
  
  val LongOrdering: Ordering[Long] = new Ordering[Long] {
    def compare(v1: Long, v2: Long): Int = v1.compare(v2)
  }
  
  val FloatOrdering: Ordering[Float] = new Ordering[Float] {
    def compare(v1: Float, v2: Float): Int = v1.compare(v2)
  }
  
  val DoubleOrdering: Ordering[Double] = new Ordering[Double] {
    def compare(v1: Double, v2: Double): Int = v1.compare(v2)
  }
  
  def Tuple2Ordering[T1, T2](implicit ordering1: Ordering[T1], ordering2: Ordering[T2]): Ordering[(T1, T2)] = new Ordering[(T1, T2)] {
    def compare(v1: (T1, T2), v2: (T1, T2)): Int = v1.compare(v2)
  }
  
  def Tuple3Ordering[T1, T2, T3](implicit ordering1: Ordering[T1], ordering2: Ordering[T2], ordering3: Ordering[T3]): Ordering[(T1, T2, T3)] = new Ordering[(T1, T2, T3)] {
    def compare(v1: (T1, T2, T3), v2: (T1, T2, T3)): Int = v1.compare(v2)
  }
  
  def Tuple4Ordering[T1, T2, T3, T4](implicit ordering1: Ordering[T1], ordering2: Ordering[T2], ordering3: Ordering[T3], ordering4: Ordering[T4]): Ordering[(T1, T2, T3, T4)] = new Ordering[(T1, T2, T3, T4)] {
    def compare(v1: (T1, T2, T3, T4), v2: (T1, T2, T3, T4)): Int = v1.compare(v2)
  }
  
  def Tuple5Ordering[T1, T2, T3, T4, T5](implicit ordering1: Ordering[T1], ordering2: Ordering[T2], ordering3: Ordering[T3], ordering4: Ordering[T4], ordering5: Ordering[T5]): Ordering[(T1, T2, T3, T4, T5)] = new Ordering[(T1, T2, T3, T4, T5)] {
    def compare(v1: (T1, T2, T3, T4, T5), v2: (T1, T2, T3, T4, T5)): Int = v1.compare(v2)
  }
  
  def OptionOrdering[T](ordering: Ordering[T]): Ordering[Option[T]] = new Ordering[Option[T]] {
    def compare(opt: Option[T], that: Option[T]): Int = {
      opt match {
        case s1 @ Some(_) => that match {
          case s2 @ Some(_) => SomeOrdering(ordering).compare(s1, s2)
          case None => 1
        }
        case None => that match {
          case Some(v2) => -1
          case None => 0
        }
      }
    }
  }
  
  def SomeOrdering[T](ordering: Ordering[T]): Ordering[Some[T]] = new Ordering[Some[T]] {
    def compare(v1: Some[T], v2: Some[T]): Int = ordering.compare(v1.get, v2.get)
  }
  
  def NoneOrdering: Ordering[None.type] = new Ordering[None.type] {
    def compare(v1: None.type, v2: None.type): Int = 0
  }
  
  def ArrayOrdering[T](ordering: Ordering[T]): Ordering[Array[T]] = new Ordering[Array[T]] {
    def compare(t1: Array[T], t2: Array[T]): Int = {
      t1.zip(t2).toList.map(t => ordering.compare(t._1, t._2)).dropWhile(_ == 0).headOption match {
        case None => t1.length.compare(t2.length)
        
        case Some(c: Int) => c
      }
    }
  }
  
  def ListOrdering[T](ordering: Ordering[T]): Ordering[List[T]] = new Ordering[List[T]] {
    def compare(v1: List[T], v2: List[T]): Int = {
      v1.zip(v2).map(t => ordering.compare(t._1, t._2)).dropWhile(_ == 0).headOption match {
        case None => v1.length.compare(v2.length)
        
        case Some(c: Int) => c
      }
    }
  }
  
  def SetOrdering[T](ordering: Ordering[T]): Ordering[Set[T]] = new Ordering[Set[T]] {
    def compare(v1: Set[T], v2: Set[T]): Int = ListOrdering(ordering).compare(v1.toList.sortWith(ordering.compare(_, _) < 0), v2.toList.sortWith(ordering.compare(_, _) < 0))
  }
  
  def MapOrdering[K, V](ordering1: Ordering[K], ordering2: Ordering[V]): Ordering[Map[K, V]] = new Ordering[Map[K, V]] {
    def compare(v1: Map[K, V], v2: Map[K, V]): Int = {
      def sorter(t1: (K, V), t2: (K, V)) = ordering1.compare(t1._1, t2._1) < 0
      
      ListOrdering(Tuple2Ordering(ordering1, ordering2)).compare(v1.toList.sortWith(sorter), v2.toList.sortWith(sorter))
    }
  }
  
  def JFieldOrdering: Ordering[JField] = new Ordering[JField] {
    def compare(v1: JField, v2: JField): Int = {
      import Stream.{cons, empty}
      
      return if (v1 == v2) 0 else {      
        val comparisons = cons(StringOrdering.compare(v1.name, v2.name), cons(JValueOrdering.compare(v1.value, v2.value), empty))
      
        comparisons.dropWhile(_ == 0).append(0 :: Nil).head
      }
    }
  }
  
  def JValueOrdering: Ordering[JValue] = new Ordering[JValue] {
    def compare(v1: JValue, v2: JValue): Int = v1 match {
      case JNothing => v2 match {
        case JNothing => 0
        case _ => -1
      }
      case JNull => v2 match {
        case JNothing => 1
        case JNull => 0
        case _ => -1
      }
      case JBool(v1) => v2 match {
        case JNothing | JNull => 1
        case JBool(v2) => v1.compare(v2)
        case _ => -1
      }
      case JInt(v1) => v2 match {
        case JNothing | JNull | JBool(_) => 1
        case JInt(v2) => v1.compare(v2)
        case _ => -1
      }
      case JDouble(v1) => v2 match {
        case JNothing | JNull | JBool(_) | JInt(_) => 1
        case JDouble(v2) => v1.compare(v2)
        case _ => -1
      }
      case JString(v1) => v2 match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) => 1
        case JString(v2) => v1.compare(v2)
        case _ => -1
      }
      case v1 @ JField(_, _) => v2 match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) | JString(_) => 1
        case v2 @ JField(_, _) => JFieldOrdering.compare(v1, v2)
        case _ => -1
      }
      case JObject(v1) => v2 match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) | JString(_) | JField(_, _) => 1
        case JObject(v2) => ListOrdering(JFieldOrdering).compare(v1, v2)
        case _ => -1
      }
      case JArray(v1) => v2 match {
        case JNothing | JNull | JBool(_) | JInt(_) | JDouble(_) | JString(_) | JField(_, _) | JObject(_) => 1
        case JArray(v2) => ListOrdering(JValueOrdering).compare(v1, v2)
        case _ => -1
      }
    }
  }
  
  def DateOrdering: Ordering[JDate] = new Ordering[JDate] {
    def compare(v1: JDate, v2: JDate) = if (v1.getTime < v2.getTime) -1 else if (v1.getTime > v2.getTime) 1 else 0
  }
}
object DefaultOrderings extends DefaultOrderings

}