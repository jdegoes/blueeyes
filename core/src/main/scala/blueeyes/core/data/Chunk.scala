package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import scalaz._

case class Chunk[A](data: A, next: Option[Future[Chunk[A]]] = None) {
  def ++(other: Chunk[A])(implicit ctx: ExecutionContext): Chunk[A] = Chunk(data, next.map(_.map(_ ++ other)).orElse(Some(Future(other))))

  def map[B](f: A => B): Chunk[B] = Chunk(f(data), next.map(_.map(_.map(f))))

  def flatten(implicit ev: A =:= Chunk[A], ctx: ExecutionContext): Chunk[A] = {
    val Chunk(d1, dn) = ev(data)
    dn match {
      case Some(fn) => Chunk(d1, next.map(_.flatMap(cc => fn.map(cn => cc.flatten ++ cn))).orElse(Some(fn)))
      case None     => Chunk(d1, next.map(_.map(_.flatten)))
    }
  }

  def flatMap[B](f: A => Chunk[B])(implicit ctx: ExecutionContext): Chunk[B] = {
    Chunk.flatten(this map f)
  }
}

object Chunk {
  def flatten[A](c: Chunk[Chunk[A]])(implicit ctx: ExecutionContext): Chunk[A] = {
    val Chunk(d1, dn) = c.data
    dn match {
      case Some(fn) => Chunk(d1, c.next.map(_.flatMap(cc => fn.map(cn => flatten(cc) ++ cn))).orElse(Some(fn)))
      case None     => Chunk(d1, c.next.map(_.map(flatten)))
    }
  }

  implicit def monad(implicit ctx: ExecutionContext): Monad[Chunk] = new Monad[Chunk] {
    def point[A](a: => A) = Chunk(a)
    override def map[A, B](c: Chunk[A])(f: A => B): Chunk[B] = c map f
    def bind[A, B](c: Chunk[A])(f: A => Chunk[B]): Chunk[B] = c flatMap f
  }
}
