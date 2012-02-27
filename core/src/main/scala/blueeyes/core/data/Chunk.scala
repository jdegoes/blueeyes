package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.ExecutionContext
import scalaz._
import scalaz.syntax.semigroup._

case class Chunk[A](data: A, next: Option[Future[Chunk[A]]] = None) {
  def ++(other: Chunk[A])(implicit ctx: ExecutionContext): Chunk[A] = Chunk(data, next.map(_.map(_ ++ other)).orElse(Some(Future(other))))

  def suffix(a: A)(implicit s: Semigroup[A]): Chunk[A] = next match {
    case None       => Chunk(data |+| a)
    case Some(more) => Chunk(data, Some(more map (_ suffix a)))
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
