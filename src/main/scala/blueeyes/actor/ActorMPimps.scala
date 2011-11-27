package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

import blueeyes.concurrent.Future

trait ActorMPimps {
  import ActorMHelpers._
  import ActorMTypeclasses._

  implicit def ActorMPimpToMAB[M[_], A, B](pimp: ActorMPimp[M, A, B]) = ActorMToMAB(pimp.value)

  implicit def ToActorMPimp[M[_]: Monad, A, B](actor: ActorM[M, A, B]) = ActorMPimp(actor)

  implicit def ToTupledOutputPimpM[M[_]: Monad, A, B1, B2, B](actor: ActorM[M, A, (B1, B2)]) = TupledOutputPimpM(actor)
  implicit def ToHigherKindedPimpM[M[_]: Monad, T[_], A, B](actor: ActorM[M, A, T[B]]) = HigherKindedPimpM(actor)
  implicit def ToValidatedPimpM[M[_]: Monad, A, E, B](actor: ActorM[M, A, Validation[E, B]]) = ValidatedPimpM(actor)
  implicit def ToHigherOrderPimpM[M[_]: Monad, A, B, C, D](actor: ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])]) = HigherOrderPimpM(actor)

  implicit def ActorMStateToActorMStatePimp[M[_]: Monad, A, B](value: ActorMState[M, A, B]): ActorMStatePimp[M, A, B] = ActorMStatePimp(value)

  sealed case class ActorMPimp[M[_], A, B](value: ActorM[M, A, B])(implicit monad: Monad[M]) extends NewType[ActorM[M, A, B]] {
    def premap[AA](f: AA => A): ActorM[M, AA, B] = receive[M, AA, B] { aa: AA =>
      (value ! f(aa)) map {
        case (b, next) =>
          (b, next.premap[AA](f))
      }
    }

    def -<- [AA](f: AA => A): ActorM[M, AA, B] = premap(f)

    def postmap[BB](f: B => BB): ActorM[M, A, BB] = value.map(f)

    def ->- [BB](f: B => BB): ActorM[M, A, BB] = value.map(f)

    def bimap[AA, BB](f: AA => A, g: B => BB): ActorM[M, AA, BB] = premap(f).postmap(g)

    def & [AA >: A, C](that: ActorM[M, AA, C]): ActorM[M, A, (B, C)] = receive { a: A =>
      (value ! a) flatMap {
        case (b, selfNext) =>
          (that ! a) map {
            case (c, thatNext) =>
              ((b, c), selfNext & thatNext)
          }
      }
    }

    def ^ [C, D] (that: ActorM[M, C, D]): ActorM[M, Either[A, C], Either[B, D]] = {
      receive { 
        case Left(a)  => (value ! a) map { case (b, value2) => (Left(b),  value2 ^ that) }
        case Right(c) => (that  ! c) map { case (d, that2)  => (Right(d), value ^ that2) }
      }
    }

    def ~> [C, D] (that: ActorM[M, (C, ActorM[M, A, B]), (D, ActorM[M, A, B])]): ActorM[M, C, D] = {
      receive { c: C =>
        (that ! ((c, value))) map {
          case ((d, this2), that2) =>
            (d, this2 ~> that2)
        }
      }
    }

    def * [AA, BB](that: ActorM[M, AA, BB]): ActorM[M, (A, AA), (B, BB)] = receive {
      case (a, aa) =>
        (value ! a) flatMap {
          case (b,  next1) =>
            (that ! aa) map {
              case (bb, next2) =>
                ((b, bb), next1 * next2)
            }
        }
    }

    def split: ActorM[M, A, (B, B)] = {
      lazy val lazySelf: ActorM[M, A, (B, B)] = receive { a: A =>
        (value ! a) map {
          case (b, next) =>
            ((b, b), lazySelf)
        }
      }

      lazySelf
    }

    def scan[Z](z: Z)(f: (Z, B) => Z): ActorM[M, A, Z] = receive { a: A =>
      (value ! a) map {
        case (b, next) =>
          val z2 = f(z, b)  

          (z2, next.scan(z2)(f))
      }
    }

    def fold[Z](z: Z, as: Seq[A])(f: (Z, B) => Z): M[(Z, ActorM[M, A, B])] = {
      if (as.length == 0) monad.pure((z, value))
      else {
        val head = as.head
        val tail = as.tail

        (value ! head) flatMap {
          case (b, next) =>
            next.fold(f(z, b), tail)(f)
        }
      }
    }

    def ifTrue[C](f: B => Boolean)(then: ActorM[M, B, C], orElse: ActorM[M, B, C]): ActorM[M, A, C] = switch(orElse)(f -> then)

    def switch[C](defaultCase: ActorM[M, B, C])(cases: (B => Boolean, ActorM[M, B, C])*): ActorM[M, A, C] = {
      def reduce(t: (B => Boolean, ActorM[M, B, C]), orElse: ActorM[M, B, C]): ActorM[M, B, C] = {
        val (p1, a1) = t

        receive { (b: B) =>
          if (p1(b)) {
            (a1 ! b) map {
              case (c, a2) =>
                (c, reduce((p1, a2), orElse))
            }
          }
          else orElse ! b
        }
      }

      value >>> (cases.foldRight[ActorM[M, B, C]](defaultCase)(reduce))
    }

    def variant[AA <: A, BB >: B]: ActorM[M, AA, BB] = value.asInstanceOf[ActorM[M, AA, BB]]
  }

  case class HigherKindedPimpM[M[_], T[_], A, B](value: ActorM[M, A, T[B]])(implicit monad: Monad[M]) extends NewType[ActorM[M, A, T[B]]] {
    def filter(f: T[B] => Boolean)(implicit empty: Empty[T]): ActorM[M, A, T[B]] = receive { a: A =>
      (value ! a) map {
        case (tb, next) =>
          (if (f(tb)) tb else implicitly[Empty[T]].empty[B], next.filter(f))
      }
    }
  }

  case class TupledOutputPimpM[M[_], A, B1, B2](value: ActorM[M, A, (B1, B2)])(implicit monad: Monad[M]) extends NewType[ActorM[M, A, (B1, B2)]] {
    def >- [B](f: (B1, B2) => B): ActorM[M, A, B] = receive { a: A =>
      (value ! a) map {
        case ((b1, b2), next) =>
          val c = f(b1, b2)

          (c, next >- (f))
      }
    }
  }

  case class ValidatedPimpM[M[_], A, E, B](value: ActorM[M, A, Validation[E, B]])(implicit monad: Monad[M]) 
      extends NewType[ActorM[M, A, Validation[E, B]]] {
    def | [AA >: A, BB <: B, EE <: E](that: ActorM[M, AA, Validation[EE, BB]]): ActorM[M, A, Validation[E, B]] = {
      receive { a: A =>
        (value ! a) flatMap {
          case (Failure(_), value2) => (that ! a) map {
            case (v, that2) => (v, value2 | that2)
          }

          case (v, value2) => monad.pure((v, value2 | that))
        }
      }
    }
  }

  case class HigherOrderPimpM[M[_], A, B, C, D](value: ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])])(implicit monad: Monad[M])
      extends NewType[ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])]] {
    def <~ (that: ActorM[M, C, D]): ActorM[M, A, B] = {
      receive { a: A =>
        (value ! ((a, that))) map {
          case ((b, that2), self2) =>
            (b, self2 <~ that2)
        }
      }
    }
  }

  case class ActorMStatePimp[M[_], A, B](value: ActorMState[M, A, B])(implicit monad: Monad[M]) extends NewType[ActorMState[M, A, B]] {
    def premap[AA](f: AA => A): ActorMState[M, AA, B] = value map { tuple =>
      tuple.mapElements[B, ActorM[M, AA, B]](identity, _.premap[AA](f))
    }
    
    def postmap[BB](f: B => BB): ActorMState[M, A, BB] = value map { tuple =>
      tuple.mapElements[BB, ActorM[M, A, BB]](f, _.postmap[BB](f))
    }

    def variant[AA <: A, BB >: B]: ActorMState[M, AA, BB] = value.asInstanceOf[M[(BB, ActorM[M, AA, BB])]]
  }
}
object ActorMPimps extends ActorMPimps