package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorTInstances {
  import ActorTFunctions._

  implicit def ActorTToMAB[M[_], A, B](actor: ActorT[M, A, B]) = mab[({type λ[a, b]=ActorT[M, a, b]})#λ, A, B](actor)

  implicit def ActorTPure[M[_], A](implicit monad: Monad[M]) = new Pure[({type λ[α]=ActorT[M, A, α]})#λ] {
    def pure[B](b: => B): ActorT[M, A, B] = {
      lazy val lazySelf: ActorT[M, A, B] = receive { a: A =>
        monad.pure((b, lazySelf))
      }

      lazySelf
    }
  }

  implicit def ActorTFunctor[M[_], A](implicit monad: Monad[M]) = new Functor[({type λ[B]=ActorT[M, A, B]})#λ] {
    def fmap[B, C](actor: ActorT[M, A, B], f: B => C): ActorT[M, A, C] = receive[M, A, C] { a: A =>
      (actor ! a) map { 
        case (b, next) =>
          (f(b), fmap(next, f))
      }
    }
  }

  implicit def ActorTCategory[M[_]](implicit monad: Monad[M]) = new Category[({type λ[a, b]=ActorT[M, a, b]})#λ] {
    def id[A] = {
      lazy val self: ActorT[M, A, A] = receive[M, A, A] { a: A => monad.pure((a, self)) }

      self
    }

    def compose[A, B, C](f: ActorT[M, B, C], g: ActorT[M, A, B]): ActorT[M, A, C] = receive[M, A, C] { a: A =>
      (g ! a) flatMap {
        case (b, next1) =>
          (f ! b) map {
            case (c, next2) =>
              (c, compose(next2, next1))
          }
      }
    }
  }

  implicit def ActorTArrow[M[_]](implicit monad: Monad[M]) = new Arrow[({type λ[a, b]=ActorT[M, a, b]})#λ] {
    val category = ActorTCategory[M]
    
    def arrow[A, B](f: A => B): ActorT[M, A, B] = moore(f)

    def first[A, B, C](actor: ActorT[M, A, B]): ActorT[M, (A, C), (B, C)] = receive[M, (A, C), (B, C)] {
      case (a, c) =>
        (actor ! a) map {
          case (b, next) =>
            ((b, c), first(next))
        }
    }

    def second[A, B, C](actor: ActorT[M, A, B]): ActorT[M, (C, A), (C, B)] = receive[M, (C, A), (C, B)] { 
      case (c, a) =>
        (actor ! a) map {
          case (b, next) =>
            ((c, b), second(next))
        }
    }
  }

  implicit def ActorTStatePure[M[_]: Monad, A] = new Pure[({type λ[α]=ActorTState[M, A, α]})#λ] {
    def pure[B](b: => B): ActorTState[M, A, B] = implicitly[Monad[M]].pure((b, ActorTPure.pure(b)))
  }
}
object ActorTInstances extends ActorTInstances