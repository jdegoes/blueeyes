import scalaz.Monoid
import scalaz.Semigroup
import scalaz.Scalaz._

package object blueeyes {
  implicit def IOClose[T <: java.io.Closeable]: util.Close[T] = new util.Close[T] {
    def close(a: T) = a.close
  }

  def lp[T](label: String) = (t: T) => println(label + ": " + t)

  implicit def KT[A](a: A): util.KT[A] = new util.KT(a)

  implicit def MapMonoid[K, V](implicit valueMonoid: Semigroup[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override val zero = Map.empty[K, V]
    override def append(m1: Map[K, V], m2: => Map[K, V]) = {
      val (from, to, semigroup) = {
        if (m1.size > m2.size) (m2, m1, (v1: V, v2: V) => valueMonoid.append(v1, v2))
        else (m1, m2, (v1: V, v2: V) => valueMonoid.append(v2, v1))
      }

      from.foldLeft(to) {
        case (to, (k, v)) => to + (k -> to.get(k).map(semigroup(_, v)).getOrElse(v))
      }
    }
  }
}

package blueeyes.util {
  trait Close[A] {
    def close(a: A): Unit
  }

  case class KT[A](a: A) {
    def ->-(f: A => Any): A = { f(a); a }
    def ->*[B](f: A => B): B = f(a)
  }
}


// vim: set ts=4 sw=4 et:
