import scalaz._
import scalaz.Scalaz._

package object blueeyes {
  implicit def IOClose[T <: java.io.Closeable]: util.Close[T] = new util.Close[T] {
    def close(a: T) = a.close
  }

  def lp[T](label: String) = (t: T) => println(label + ": " + t)

  def lpf[T](label: String)(f: T => Any) = (t: T) => println(label + ": " + f(t))

  implicit def K[A](a: A): util.K[A] = new util.K(a)
}

package blueeyes.util {
  trait Close[A] {
    def close(a: A): Unit
  }

  case class K[A](a: A) {
    def ->-(f: A => Any): A = { f(a); a }
  }
}


// vim: set ts=4 sw=4 et:
