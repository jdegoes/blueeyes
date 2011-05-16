package object blueeyes {
  implicit def IOClose[T <: java.io.Closeable]: util.Close[T] = new util.Close[T] {
    def close(a: T) = a.close
  }

  def lp[T](label: String) = (t: T) => println(label + ": " + t)

  implicit def KT[A](a: A): util.KT[A] = new util.KT(a)
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
