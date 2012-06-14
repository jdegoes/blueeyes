package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import scalaz._
import scalaz.syntax.semigroup._

trait Incremental[A] extends Semigroup[A] {
  def length(a: A): Int
  def isEmpty(a: A): Boolean = length(a) == 0
  def splitAt(a: A, i: Int): (A, A)
}

case class Chunk[A](data: A, next: Option[Future[Chunk[A]]] = None) {
  def isEOF = next.isEmpty

  def ++(other: Chunk[A])(implicit ctx: ExecutionContext): Chunk[A] = Chunk(data, next.map(_.map(_ ++ other)).orElse(Some(Future(other))))

  def suffix(a: A)(implicit s: Semigroup[A]): Chunk[A] = next match {
    case None       => Chunk(data |+| a)
    case Some(more) => Chunk(data, Some(more map (_ suffix a)))
  }

  /**
   * Extends the given chunk by suffixing with the given value. This will attempt to accumulate
   * as much data in each chunk as possible, up to the specified limit. This will not attempt to split
   * single elements that exceed the maximum chunk size.
   */
  def extend(maxSize: Int)(a: A)(implicit s: Semigroup[A], len: A => Int, ctx: ExecutionContext): Chunk[A] = next match {
    case None if len(data) + len(a) < maxSize => Chunk(data |+| a)
    case None                                 => Chunk(data, Some(Future(Chunk(a))))
    case Some(more)                           => Chunk(data, Some(more map (_.extend(maxSize)(a))))
  }

  /**
   * Extend the given chunk by suffixing with the given value. This will attempt to split single elements
   * that exceed the maximum chunk size; however, it is important to note that if the specified Incremental
   * is not able to split the specified value (i.e. returns all of the data of an element in a suffix) then
   * this will result in a runtime error.
   */
  def extendSplit(maxSize: Int)(a: A)(implicit incr: Incremental[A], ctx: ExecutionContext): Chunk[A] = {
    val dlen = incr.length(data)
    val alen = incr.length(a)
    next match {
      case Some(more)                    => Chunk(data, Some(more map (_.extendSplit(maxSize)(a))))
      case None if dlen + alen < maxSize => Chunk(data |+| a)
      case None                          =>
        val (prefix, suffix) = incr.splitAt(a, maxSize - dlen)
        val sufLen = incr.length(suffix)

        if ((maxSize - dlen) > 0 && sufLen >= alen)         
          sys.error("Unable to split value into chunks; halting now to prevent unbounded recursion.") 
        else if (sufLen < maxSize)  
          Chunk(data |+| prefix, Some(Future(Chunk(suffix))))
        else {
          val (sufPref, sufSuf) = incr.splitAt(suffix, maxSize)
          Chunk(data |+| prefix, Some(Future(Chunk(sufPref).extendSplit(maxSize)(sufSuf))))
        }
    }
  }

  def map[B](f: A => B): Chunk[B] = Chunk(f(data), next.map(_.map(_.map(f))))

  def flatten(implicit ev: A =:= Chunk[A], ctx: ExecutionContext): Chunk[A] = Chunk.flatten(this map ev)

  def flatMap[B](f: A => Chunk[B])(implicit ctx: ExecutionContext): Chunk[B] = {
    Chunk.flatten(this map f)
  }
}

object Chunk {
  def flatten[A](c: Chunk[Chunk[A]])(implicit ctx: ExecutionContext): Chunk[A] = {
    val Chunk(d1, dn) = c.data
    dn match {
      case Some(more) => Chunk(d1, c.next.map(_.flatMap(cc => more.map(cn => flatten(cc) ++ cn))).orElse(Some(more)))
      case None       => Chunk(d1, c.next.map(_.map(flatten)))
    }
  }

  implicit def monad(implicit ctx: ExecutionContext): Monad[Chunk] = new Monad[Chunk] {
    def point[A](a: => A) = Chunk(a)
    override def map[A, B](c: Chunk[A])(f: A => B): Chunk[B] = c map f
    def bind[A, B](c: Chunk[A])(f: A => Chunk[B]): Chunk[B] = c flatMap f
  }
}
