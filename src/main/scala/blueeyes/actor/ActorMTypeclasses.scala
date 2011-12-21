package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorMTypeclasses {
  import ActorMHelpers._

  //implicit def ActorMToMAB[M[_], A, B](actor: ActorM[M, A, B]) = mab[({type λ[a, b]=ActorM[M, a, b]})#λ, A, B](actor)

  implicit def ActorMPointed[M[_], A](implicit monad: Monad[M]) = new Pointed[({type λ[α]=ActorM[M, A, α]})#λ] {
    def point[B](b: => B): ActorM[M, A, B] = {
      lazy val lazySelf: ActorM[M, A, B] = receive { a: A =>
        monad.point((b, lazySelf))
      }

      lazySelf
    }
  }

  implicit def ActorMApply[M[_], A](implicit monad: Monad[M]) = new Apply[({type λ[α]=ActorM[M, A, α]})#λ] {
    def apply[B, C](actor1: ActorM[M, A, B => C], actor2: ActorM[M, A, B]): ActorM[M, A, C] = receive { a: A =>
      (actor1 ! a) flatMap {
        case (bToC, next1) =>
          (actor2 ! a) map {
            case (b, next2) =>
              val c = bToC(b)

              (c, apply(next1, next2))
          }
      }
    }
  }

  implicit def ActorMCategory[M[_]](implicit monad: Monad[M]) = new Category[({type λ[a, b]=ActorM[M, a, b]})#λ] {
    def id[A] = {
      lazy val self: ActorM[M, A, A] = receive[M, A, A] { a: A => monad.point((a, self)) }

      self
    }

    def compose[A, B, C](f: ActorM[M, B, C], g: ActorM[M, A, B]): ActorM[M, A, C] = receive[M, A, C] { a: A =>
      (g ! a) flatMap {
        case (b, next1) =>
          (f ! b) map {
            case (c, next2) =>
              (c, compose(next2, next1))
          }
      }
    }
  }

  implicit def ActorMArrow[M[_]](implicit monad: Monad[M]) = new Arrow[({type λ[a, b]=ActorM[M, a, b]})#λ] {
    val category = ActorMCategory[M]
    
    def arrow[A, B](f: A => B): ActorM[M, A, B] = moore(f)

    def first[A, B, C](actor: ActorM[M, A, B]): ActorM[M, (A, C), (B, C)] = receive[M, (A, C), (B, C)] {
      case (a, c) =>
        (actor ! a) map {
          case (b, next) =>
            ((b, c), first(next))
        }
    }

    def second[A, B, C](actor: ActorM[M, A, B]): ActorM[M, (C, A), (C, B)] = receive[M, (C, A), (C, B)] { 
      case (c, a) =>
        (actor ! a) map {
          case (b, next) =>
            ((c, b), second(next))
        }
    }
  }

  implicit def ActorMApplicative[M[_]: Monad, A]: Applicative[({type λ[α]=ActorM[M, A, α]})#λ] = 
    Applicative.applicative[({type λ[α] = ActorM[M, A, α]})#λ](ActorMPointed, ActorMApply)

  implicit def ActorMStatePointed[M[_], A](implicit monad: Monad[M]) = new Pointed[({type λ[α]=ActorMState[M, A, α]})#λ] {
    def point[B](b: => B): ActorMState[M, A, B] = monad.point((b, ActorMPointed(monad).point(b)))
  }

  implicit def ActorMStateApply[M[_], A](implicit monad: Monad[M]) = new Apply[({type λ[α]=ActorMState[M, A, α]})#λ] {
    def apply[B, C](state1: ActorMState[M, A, B => C], state2: ActorMState[M, A, B]): ActorMState[M, A, C] = {
      state1 flatMap {
        case (bToC, next1) =>
          state2 map {
            case (b, next2) =>
              (bToC(b), ActorMApply(monad).apply(next1, next2))
          }
      }
    }
  }

  implicit def ActorMStateApplicative[M[_]: Monad, A]: Applicative[({type λ[α]=ActorMState[M, A, α]})#λ] = 
    Applicative.applicative[({type λ[α] = ActorMState[M, A, α]})#λ](ActorMStatePointed, ActorMStateApply)
}
object ActorMTypeclasses extends ActorMTypeclasses
