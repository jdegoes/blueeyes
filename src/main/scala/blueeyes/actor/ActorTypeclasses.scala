package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorTypeclasses {
  import ActorHelpers._

  trait ActorPointed[A] extends Pointed[({type λ[α]=Actor[A, α]})#λ] {
    def point[B](b: => B): Actor[A, B] = {
      lazy val lazySelf: Actor[A, B] = receive { a: A =>
        (b, lazySelf)
      }

      lazySelf
    }
  }


  implicit def actorPointed[A] = new ActorPointed[A] { }

  trait ActorApply[A] extends Apply[({type λ[α]=Actor[A, α]})#λ] {
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

  implicit def actorApply[A] = new ActorApply[A] { }

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

  implicit def ActorApplicative[A]: Applicative[({type λ[α]=Actor[A, α]})#λ] = 
    new Applicative[({type λ[α]=Actor[A, α]})#λ] with ActorPointed[A] with ActorApply[A] { }

  trait ActorStatePointed[A] extends Pointed[({type λ[α]=ActorState[A, α]})#λ] {
    def point[B](b: => B): ActorState[A, B] = (b, actorPointed.point(b))
  }

  implicit def actorStatePointed[A] = new ActorStatePointed[A] { } 

  trait ActorStateApply[A] extends Apply[({type λ[α]=ActorState[A, α]})#λ] {
    def apply[B, C](state1: ActorState[A, B => C], state2: ActorState[A, B]): ActorState[A, C] = {
      val (bToC, next1) = state1
      val (b,    next2) = state2

      (bToC(b), actorApply.apply(next1, next2))
    }
  }

  implicit def actorStateApply[A] = new ActorStateApply[A] { }

  implicit def ActorStateApplicative[A]: Applicative[({type λ[α]=ActorState[A, α]})#λ] = 
    new Applicative[({type λ[α] = ActorState[A, α]})#λ] with ActorStatePointed[A] with ActorStateApply[A] { }
}
object ActorTypeclasses extends ActorTypeclasses
