package blueeyes.core.data

import akka.dispatch.Future

trait AsyncTranscoder[F[_], G[_], A, B] {
  def apply(a: F[A]): F[B]
  def unapply(fb: Future[G[B]]): Future[G[A]]
}

// vim: set ts=4 sw=4 et:
