package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

import blueeyes.concurrent.Future

trait ActorTPimps {
  import ActorTFunctions._
  import ActorTInstances._

  implicit def ActorTPimpToMAB[M[_], A, B](pimp: ActorTPimp[M, A, B]) = ActorTToMAB(pimp.value)

  implicit def ToActorTPimp[M[_]: Monad, A, B](actor: ActorT[M, A, B]) = ActorTPimp(actor)

  implicit def ToTupledOutputPimpM[M[_]: Monad, A, B1, B2, B](actor: ActorT[M, A, (B1, B2)]) = TupledOutputPimpM(actor)
  implicit def ToHigherKindedPimpM[M[_]: Monad, T[_], A, B](actor: ActorT[M, A, T[B]]) = HigherKindedPimpM(actor)
  implicit def ToValidatedPimpM[M[_]: Monad, A, E, B](actor: ActorT[M, A, Validation[E, B]]) = ValidatedPimpM(actor)
  implicit def ToHigherOrderPimpM[M[_]: Monad, A, B, C, D](actor: ActorT[M, (A, ActorT[M, C, D]), (B, ActorT[M, C, D])]) = HigherOrderPimpM(actor)

  implicit def ActorTStateToActorTStatePimp[M[_]: Monad, A, B](value: ActorTState[M, A, B]): ActorTStatePimp[M, A, B] = ActorTStatePimp(value)

  sealed case class ActorTPimp[M[_], A, B](value: ActorT[M, A, B])(implicit monad: Monad[M]) extends NewType[ActorT[M, A, B]] {
    def premap[AA](f: AA => A): ActorT[M, AA, B] = receive[M, AA, B] { aa: AA =>
      (value ! f(aa)) map {
        case (b, next) =>
          (b, next.premap[AA](f))
      }
    }

    def -<- [AA](f: AA => A): ActorT[M, AA, B] = premap(f)

    def postmap[BB](f: B => BB): ActorT[M, A, BB] = value.map(f)

    def ->- [BB](f: B => BB): ActorT[M, A, BB] = value.map(f)

    def bimap[AA, BB](f: AA => A, g: B => BB): ActorT[M, AA, BB] = premap(f).postmap(g)

    def & [AA >: A, C](that: ActorT[M, AA, C]): ActorT[M, A, (B, C)] = receive { a: A =>
      (value ! a) flatMap {
        case (b, selfNext) =>
          (that ! a) map {
            case (c, thatNext) =>
              ((b, c), selfNext & thatNext)
          }
      }
    }

    def ^ [C, D] (that: ActorT[M, C, D]): ActorT[M, Either[A, C], Either[B, D]] = {
      receive { 
        case Left(a)  => (value ! a) map { case (b, value2) => (Left(b),  value2 ^ that) }
        case Right(c) => (that  ! c) map { case (d, that2)  => (Right(d), value ^ that2) }
      }
    }

    def ~> [C, D] (that: ActorT[M, (C, ActorT[M, A, B]), (D, ActorT[M, A, B])]): ActorT[M, C, D] = {
      receive { c: C =>
        (that ! ((c, value))) map {
          case ((d, this2), that2) =>
            (d, this2 ~> that2)
        }
      }
    }

    def * [AA, BB](that: ActorT[M, AA, BB]): ActorT[M, (A, AA), (B, BB)] = receive {
      case (a, aa) =>
        (value ! a) flatMap {
          case (b,  next1) =>
            (that ! aa) map {
              case (bb, next2) =>
                ((b, bb), next1 * next2)
            }
        }
    }

    def split: ActorT[M, A, (B, B)] = {
      lazy val lazySelf: ActorT[M, A, (B, B)] = receive { a: A =>
        (value ! a) map {
          case (b, next) =>
            ((b, b), lazySelf)
        }
      }

      lazySelf
    }

    def scan[Z](z: Z)(f: (Z, B) => Z): ActorT[M, A, Z] = receive { a: A =>
      (value ! a) map {
        case (b, next) =>
          val z2 = f(z, b)  

          (z2, next.scan(z2)(f))
      }
    }

    def fold[Z](z: Z, as: Seq[A])(f: (Z, B) => Z): M[(Z, ActorT[M, A, B])] = {
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

    def ifTrue[C](f: B => Boolean)(then: ActorT[M, B, C], orElse: ActorT[M, B, C]): ActorT[M, A, C] = switch(orElse)(f -> then)

    def switch[C](defaultCase: ActorT[M, B, C])(cases: (B => Boolean, ActorT[M, B, C])*): ActorT[M, A, C] = {
      def reduce(t: (B => Boolean, ActorT[M, B, C]), orElse: ActorT[M, B, C]): ActorT[M, B, C] = {
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

      value >>> (cases.foldRight[ActorT[M, B, C]](defaultCase)(reduce))
    }

    def variant[AA <: A, BB >: B]: ActorT[M, AA, BB] = value.asInstanceOf[ActorT[M, AA, BB]]
  }

  case class HigherKindedPimpM[M[_], T[_], A, B](value: ActorT[M, A, T[B]])(implicit monad: Monad[M]) extends NewType[ActorT[M, A, T[B]]] {
    def filter(f: T[B] => Boolean)(implicit empty: Empty[T]): ActorT[M, A, T[B]] = receive { a: A =>
      (value ! a) map {
        case (tb, next) =>
          (if (f(tb)) tb else implicitly[Empty[T]].empty[B], next.filter(f))
      }
    }
  }

  case class TupledOutputPimpM[M[_], A, B1, B2](value: ActorT[M, A, (B1, B2)])(implicit monad: Monad[M]) extends NewType[ActorT[M, A, (B1, B2)]] {
    def >- [B](f: (B1, B2) => B): ActorT[M, A, B] = receive { a: A =>
      (value ! a) map {
        case ((b1, b2), next) =>
          val c = f(b1, b2)

          (c, next >- (f))
      }
    }
  }

  case class ValidatedPimpM[M[_], A, E, B](value: ActorT[M, A, Validation[E, B]])(implicit monad: Monad[M]) 
      extends NewType[ActorT[M, A, Validation[E, B]]] {
    def | [AA >: A, BB <: B, EE <: E](that: ActorT[M, AA, Validation[EE, BB]]): ActorT[M, A, Validation[E, B]] = {
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

  case class HigherOrderPimpM[M[_], A, B, C, D](value: ActorT[M, (A, ActorT[M, C, D]), (B, ActorT[M, C, D])])(implicit monad: Monad[M])
      extends NewType[ActorT[M, (A, ActorT[M, C, D]), (B, ActorT[M, C, D])]] {
    def <~ (that: ActorT[M, C, D]): ActorT[M, A, B] = {
      receive { a: A =>
        (value ! ((a, that))) map {
          case ((b, that2), self2) =>
            (b, self2 <~ that2)
        }
      }
    }
  }

  case class ActorTStatePimp[M[_], A, B](value: ActorTState[M, A, B])(implicit monad: Monad[M]) extends NewType[ActorTState[M, A, B]] {
    def premap[AA](f: AA => A): ActorTState[M, AA, B] = value map { tuple =>
      tuple.mapElements[B, ActorT[M, AA, B]](identity, _.premap[AA](f))
    }
    
    def postmap[BB](f: B => BB): ActorTState[M, A, BB] = value map { tuple =>
      tuple.mapElements[BB, ActorT[M, A, BB]](f, _.postmap[BB](f))
    }

    def variant[AA <: A, BB >: B]: ActorTState[M, AA, BB] = value.asInstanceOf[M[(BB, ActorT[M, AA, BB])]]
  }
}
object ActorTPimps extends ActorTPimps