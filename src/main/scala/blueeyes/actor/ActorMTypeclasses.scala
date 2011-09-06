package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

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

  implicit def ActorMApply[M[_], A](implicit monad: Monad[M]) = new Apply[({type λ[α]=ActorM[M, A, α]})#λ] {
    // Holy shit, batman!
    def apply[B, C](actor1: ActorM[M, A, B => C], actor2: ActorM[M, A, B]): ActorM[M, A, C] = receive { a: A =>
      ((actor1 ! a), (actor2 ! a)) match {
        case ((bToC, next1), (b, next2)) =>
          unwrapM(b.map { b: B =>
            unwrapM(bToC.map { bToC =>
              val c = bToC(b)

              (monad.pure(c), apply(next1, next2))
            })
          })
      }
    }
  }

  implicit def ActorMCategory[M[_]](implicit monad: Monad[M]) = new Category[({type λ[α, β]=ActorM[M, α, β]})#λ] {
    def id[A] = {
      lazy val lazySelf: ActorM[M, A, A] = receive[M, A, A] { a: A => (monad.pure(a), lazySelf) }

      lazySelf
    }

    def compose[A, B, C](f: ActorM[M, B, C], g: ActorM[M, A, B]): ActorM[M, A, C] = receive[M, A, C] { a: A =>
      (g ! a) match {
        case (b, next1) =>
          unwrapM[M, A, C](b.map { b: B =>
            (f ! b) match {
              case (c, next2) => (c, compose(next2, next1))
            }
          })
      }
    }
  }

  implicit def ActorMArrow[M[_]](implicit monad: Monad[M]) = new Arrow[({type λ[α, β]=ActorM[M, α, β]})#λ] {
    val category = ActorMCategory
    
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

  implicit def ActorMApplicative[M[_], A](implicit monad: Monad[M]): Applicative[({type λ[α]=ActorM[M, A, α]})#λ] = 
    Applicative.applicative[({type λ[α] = ActorM[M, A, α]})#λ](ActorMPure[M, A], ActorMApply[M, A])

  implicit def ActorMStatePure[M[_], A](implicit monad: Monad[M]) = new Pure[({type λ[α]=ActorMState[M, A, α]})#λ] {
    def pure[B](b: => B): ActorMState[M, A, B] = (monad.pure(b), ActorMPure.pure(b))
  }

  implicit def ActorMStateApply[M[_], A](implicit monad: Monad[M]) = new Apply[({type λ[α]=ActorMState[M, A, α]})#λ] {
    def apply[B, C](state1: ActorMState[M, A, B => C], state2: ActorMState[M, A, B]): ActorMState[M, A, C] = {
      val (bToCM, next1) = state1
      val (bM,    next2) = state2

      (for (bToC <- bToCM; b <- bM) yield bToC(b), ActorMApply.apply(next1, next2))
    }
  }

  implicit def ActorMStateApplicative[M[_], A](implicit monad: Monad[M]): Applicative[({type λ[α]=ActorMState[M, A, α]})#λ] = 
    Applicative.applicative[({type λ[α] = ActorMState[M, A, α]})#λ](ActorMStatePure[M, A], ActorMStateApply[M, A])
}
object ActorMTypeclasses extends ActorMTypeclasses