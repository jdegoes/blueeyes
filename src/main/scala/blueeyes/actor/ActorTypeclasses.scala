package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorTypeclasses {
  import ActorHelpers._

  implicit def ActorToMAB[M[_], A, B](actor: Actor[A, B]) = mab[Actor, A, B](actor)

  implicit def ActorPure[A] = new Pure[({type λ[α]=Actor[A, α]})#λ] {
    def pure[B](b: => B): Actor[A, B] = {
      lazy val lazySelf: Actor[A, B] = receive { a: A =>
        (b, lazySelf)
      }

      lazySelf
    }
  }

  implicit def ActorApply[A] = new Apply[({type λ[α]=Actor[A, α]})#λ] {
    def apply[B, C](actor1: Actor[A, B => C], actor2: Actor[A, B]): Actor[A, C] = receive { a: A =>
      (actor1 ! a) match {
        case (bToC, next1) =>
          (actor2 ! a) match {
            case (b, next2) =>
              val c = bToC(b)

              (c, apply(next1, next2))
          }
      }
    }
  }

  implicit val ActorCategory = new Category[Actor] {
    def id[A] = {
      lazy val self: Actor[A, A] = receive[A, A] { a: A => (a, self) }

      self
    }

    def compose[A, B, C](f: Actor[B, C], g: Actor[A, B]): Actor[A, C] = receive[A, C] { a: A =>
      (g ! a) match {
        case (b, next1) =>
          (f ! b) match {
            case (c, next2) =>
              (c, compose(next2, next1))
          }
      }
    }
  }

  implicit val ActorArrow = new Arrow[Actor] {
    val category = ActorCategory
    
    def arrow[A, B](f: A => B): Actor[A, B] = moore(f)

    def first[A, B, C](actor: Actor[A, B]): Actor[(A, C), (B, C)] = receive[(A, C), (B, C)] {
      case (a, c) =>
        (actor ! a) match {
          case (b, next) =>
            ((b, c), first(next))
        }
    }

    def second[A, B, C](actor: Actor[A, B]): Actor[(C, A), (C, B)] = receive[(C, A), (C, B)] { 
      case (c, a) =>
        (actor ! a) match {
          case (b, next) =>
            ((c, b), second(next))
        }
    }
  }

  implicit def ActorApplicative[A]: Applicative[({type λ[α]=Actor[A, α]})#λ] = Applicative.applicative[({type λ[α] = Actor[A, α]})#λ](ActorPure, ActorApply)

  implicit def ActorStatePure[A] = new Pure[({type λ[α]=ActorState[A, α]})#λ] {
    def pure[B](b: => B): ActorState[A, B] = (b, ActorPure.pure(b))
  }

  implicit def ActorStateApply[A] = new Apply[({type λ[α]=ActorState[A, α]})#λ] {
    def apply[B, C](state1: ActorState[A, B => C], state2: ActorState[A, B]): ActorState[A, C] = {
      val (bToC, next1) = state1
      val (b,    next2) = state2

      (bToC(b), ActorApply.apply(next1, next2))
    }
  }

  implicit def ActorStateApplicative[A]: Applicative[({type λ[α]=ActorState[A, α]})#λ] = 
    Applicative.applicative[({type λ[α] = ActorState[A, α]})#λ](ActorStatePure, ActorStateApply)
}
object ActorTypeclasses extends ActorTypeclasses