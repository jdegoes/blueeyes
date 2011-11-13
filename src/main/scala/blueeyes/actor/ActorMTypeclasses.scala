package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

import blueeyes.concurrent.Future

trait ActorMTypeclasses {
  import ActorMHelpers._

  implicit def ActorMToMAB[M[_], A, B](actor: ActorM[M, A, B]) = mab[({type λ[α, β]=ActorM[M, α, β]})#λ, A, B](actor)

  implicit def ActorMPure[M[_], A](implicit monad: Monad[M]) = new Pure[({type λ[α]=ActorM[M, A, α]})#λ] {
    def pure[B](b: => B): ActorM[M, A, B] = {
      lazy val lazySelf: ActorM[M, A, B] = receive { a: A =>
        (monad.pure(b), lazySelf)
      }

      lazySelf
    }
  }

  implicit def ActorMFunctor[M[_], A](implicit monad: Monad[M]) = new Functor[({type λ[α]=ActorM[M, A, α]})#λ] {
    def fmap[B, C](actor: ActorM[M, A, B], f: B => C): ActorM[M, A, C] = receive { a: A =>
      val (mb, next) = (actor ! a)

      (mb.map(f), fmap(next, f))
    }
  }

  implicit def ActorOptionApply[A](implicit monad: Monad[Option]) = new Apply[({type λ[α]=ActorM[Option, A, α]})#λ] {
    def apply[B, C](actor1: ActorM[Option, A, B => C], actor2: ActorM[Option, A, B]): ActorM[Option, A, C] = receive { a: A =>
      ((actor1 ! a), (actor2 ! a)) match {
        case ((Some(bToC), next1), (Some(b), next2)) =>
          (monad.pure(bToC(b)), apply(next1, next2))

        case ((_, next1), (_, next2)) =>
          (None, apply(next1, next2))
      }
    }
  }

  implicit def ActorFutureCategory(implicit monad: Monad[Future]) = new Category[({type λ[α, β]=ActorM[Future, α, β]})#λ] {
    def id[A] = {
      lazy val lazySelf: ActorM[Future, A, A] = receive[Future, A, A] { a: A => (monad.pure(a), lazySelf) }

      lazySelf
    }

    def compose[A, B, C](f: ActorM[Future, B, C], g: ActorM[Future, A, B]): ActorM[Future, A, C] = {
      // TODO
      throw new Exception("Not implemented")
    }
  }

  implicit def ActorOptionCategory(implicit monad: Monad[Option]) = new Category[({type λ[α, β]=ActorM[Option, α, β]})#λ] {
    def id[A] = {
      lazy val lazySelf: ActorM[Option, A, A] = receive[Option, A, A] { a: A => (monad.pure(a), lazySelf) }

      lazySelf
    }

    def compose[A, B, C](f: ActorM[Option, B, C], g: ActorM[Option, A, B]): ActorM[Option, A, C] = receive[Option, A, C] { a: A =>
      val (mb, next1) = g ! a

      mb.map { b =>
        val (mc, next2) = f ! b

        (mc, compose(next2, next1))
      }.getOrElse {
        (None, compose(f, next1))
      }
    }
  }

  implicit def ActorMArrow[M[_]](implicit monad: Monad[M], cat: Category[({type λ[α, β]=ActorM[M, α, β]})#λ]) = new Arrow[({type λ[α, β]=ActorM[M, α, β]})#λ] {
    val category = cat
    
    def arrow[A, B](f: A => B): ActorM[M, A, B] = moore(f)

    def first[A, B, C](actor: ActorM[M, A, B]): ActorM[M, (A, C), (B, C)] = receive[M, (A, C), (B, C)] {
      case (a, c) =>
        (actor ! a) match {
          case (mb, next) =>
            (mb.map(b => (b, c)), first(next))
        }
    }

    def second[A, B, C](actor: ActorM[M, A, B]): ActorM[M, (C, A), (C, B)] = receive[M, (C, A), (C, B)] { 
      case (c, a) =>
        (actor ! a) match {
          case (mb, next) =>
            (mb.map(b => (c, b)), second(next))
        }
    }
  }

  implicit def ActorMStatePure[M[_], A](implicit monad: Monad[M]) = new Pure[({type λ[α]=ActorMState[M, A, α]})#λ] {
    def pure[B](b: => B): ActorMState[M, A, B] = (monad.pure(b), ActorMPure.pure(b))
  }
}
object ActorMTypeclasses extends ActorMTypeclasses