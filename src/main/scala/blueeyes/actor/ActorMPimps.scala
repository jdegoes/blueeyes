package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorMPimps {
  import ActorMHelpers._
  import ActorMTypeclasses._

  implicit def ActorMPimpToMAB[M[_], A, B](pimp: ActorMPimp[M, A, B]) = ActorMToMAB(pimp.value)

  implicit def ToActorMPimp[M[_], A, B](actor: ActorM[M, A, B]) = ActorMPimp(actor)

  implicit def ToTupledOutputPimpM[M[_], A, B1, B2, B](actor: ActorM[M, A, (B1, B2)]) = TupledOutputPimpM(actor)
  implicit def ToHigherKindedPimpM[M[_], T[_], A, B](actor: ActorM[M, A, T[B]]) = HigherKindedPimpM(actor)
  implicit def ToValidatedPimpM[M[_], A, E, B](actor: ActorM[M, A, Validation[E, B]]) = ValidatedPimpM(actor)
  implicit def ToHigherOrderPimp[M[_], A, B, C, D](actor: ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])]) = HigherOrderPimpM(actor)

  implicit def ToActorMStatePimp[M[_], A, B](value: ActorMState[M, A, B]): ActorMStatePimp[M, A, B] = ActorMStatePimp(value)

  sealed case class ActorMPimp[M[_], A, B](value: ActorM[M, A, B]) extends NewType[ActorM[M, A, B]] {
    def premap[AA](f: AA => A): ActorM[M, AA, B] = receive { aa: AA =>
      (value ! f(aa)).mapElements(identity, _.premap(f))
    }

     def -<- [AA](f: AA => A): ActorM[M, AA, B] = premap(f)

    def postmap[BB](f: B => BB)(implicit monad: Monad[M]): ActorM[M, A, BB] = value.map(f)

    def ->- [BB](f: B => BB)(implicit monad: Monad[M]): ActorM[M, A, BB] = value.map(f)

    def bimap[AA, BB](f: AA => A, g: B => BB)(implicit monad: Monad[M]): ActorM[M, AA, BB] = premap(f).postmap(g)

    def & [AA >: A, C](that: ActorM[M, AA, C])(implicit monad: Monad[M]): ActorM[M, A, (B, C)] = receive { a: A =>
      val (bM, selfNext) = value ! a
      val (cM, thatNext) = that ! a

      (for (b <- bM; c <- cM) yield (b, c), selfNext & thatNext)
    }

    def variant[AA <: A, BB >: B](implicit monad: Monad[M]): ActorM[M, AA, BB] = premap[AA](aa => (aa: A)).postmap[BB](b => (b: BB))

    def ^ [C, D] (that: ActorM[M, C, D])(implicit functor: Functor[M]): ActorM[M, Either[A, C], Either[B, D]] = {
      receive { 
        case Left(a)  => (value ! a).mapElements(_.map(Left.apply  _), _ ^ that)
        case Right(c) => (that  ! c).mapElements(_.map(Right.apply _), value ^ _)
      }
    }

    def ~> [C, D] (that: ActorM[M, (C, ActorM[M, A, B]), (D, ActorM[M, A, B])])(implicit monad: Monad[M]): ActorM[M, C, D] = {
      receive { c: C =>
        val (mv, that2) = that ! ((c, value))

        unwrapM(mv.map {
          case (d, this2) => (monad.pure(d), this2 ~> that2)
        })
      }
    }
    
    def * [AA, BB](that: ActorM[M, AA, BB])(implicit monad: Monad[M]): ActorM[M, (A, AA), (B, BB)] = receive {
      case (a, aa) =>
        val (bM,  next1) = value ! a
        val (bbM, next2) = that ! aa

        (for (b <- bM; bb <- bbM) yield (b, bb), next1 * next2)
    }

    def split(implicit monad: Monad[M]): ActorM[M, A, (B, B)] = {
      lazy val lazySelf: ActorM[M, A, (B, B)] = receive { a: A =>
        val (bM, next) = value ! a

        (for (b <- bM) yield (b, b), lazySelf)
      }

      lazySelf
    }

    def scan[Z](z: Z)(f: (Z, B) => Z)(implicit monad: Monad[M]): ActorM[M, A, Z] = {
      def scan0(mz: M[Z])(f: (Z, B) => Z): ActorM[M, A, Z] = {
        receive { a: A =>
          val (mb, next) = value ! a

          val mz2 = for (b <- mb; z <- mz) yield f(z, b)

          unwrapM[M, A, Z](for (z2 <- mz2) yield (mz2, next.scan(z2)(f)))
        }
      }

      scan0(monad.pure(z))(f)
    } 

    def fold[Z](z: Z, as: Seq[A])(f: (Z, B) => Z)(implicit monad: Monad[M]): (M[Z], ActorM[M, A, B]) = {
      as.foldLeft[(M[Z], ActorM[M, A, B])]((monad.pure(z), value)) {
        case ((mz, actor), a) =>
          val (mb, actor2) = actor ! a

          (for (z <- mz; b <- mb) yield f(z, b), actor2)
      }
    }

    def ifTrue[C](f: B => Boolean)(then: ActorM[M, B, C], orElse: ActorM[M, B, C])(implicit monad: Monad[M]): ActorM[M, A, C] = switch(orElse)(f -> then)

    def switch[C](defaultCase: ActorM[M, B, C])(cases: (B => Boolean, ActorM[M, B, C])*)(implicit monad: Monad[M]): ActorM[M, A, C] = {
      def reduce(t: (B => Boolean, ActorM[M, B, C]), orElse: ActorM[M, B, C]): ActorM[M, B, C] = {
        val (p1, a1) = t

        receive { (b: B) =>
          if (p1(b)) {
            val (c, a2) = a1 ! b

            (c, reduce((p1, a2), orElse))
          }
          else orElse ! b
        }
      }

      value >>> (cases.foldRight[ActorM[M, B, C]](defaultCase)(reduce))
    }
  }

  case class HigherKindedPimpM[M[_], T[_], A, B](value: ActorM[M, A, T[B]]) extends NewType[ActorM[M, A, T[B]]] {
    def filter(f: T[B] => Boolean)(implicit monad: Monad[M], empty: Empty[T]): ActorM[M, A, T[B]] = receive { a: A =>
      val (mtb, next) = value ! a

      (for (tb <- mtb) yield if (f(tb)) tb else implicitly[Empty[T]].empty[B], next.filter(f))
    }
  }

  case class TupledOutputPimpM[M[_], A, B1, B2](value: ActorM[M, A, (B1, B2)]) extends NewType[ActorM[M, A, (B1, B2)]] {
    def >- [B](f: (B1, B2) => B)(implicit monad: Monad[M]): ActorM[M, A, B] = receive { a: A =>
      val (mv, next) = value ! a

      unwrapM(mv.map {
        case ((b1, b2)) =>
          val c = f(b1, b2)

          (monad.pure(c), next >- (f))
      })
    }
  }

  case class ValidatedPimpM[M[_], A, E, B](value: ActorM[M, A, Validation[E, B]]) extends NewType[ActorM[M, A, Validation[E, B]]] {
    def | [AA >: A, BB <: B, EE <: E](that: ActorM[M, AA, Validation[EE, BB]])(implicit monad: Monad[M]): ActorM[M, A, Validation[E, B]] = {
      receive { a: A =>
        val (result, next) = value ! a

        unwrapM(result.map { 
          case Failure(_) => (that ! a).variant[A, Validation[E, B]].mapElements(identity, next | _)

          case x => (monad.pure(x), next | that.variant[A, Validation[E, B]])
        })
      }
    }

    def || [AA >: A, BB <: B, EE <: E](that: ActorM[M, AA, Validation[EE, BB]])(implicit monad: Monad[M]): ActorM[M, A, Validation[E, B]] = {
      receive { a: A =>
        val (result1, self2) = value ! a
        val (result2, that2) = (that ! a).variant[A, Validation[E, B]]

        unwrapM(result1.map { 
          case Failure(_) => (result2, self2 || that2)

          case x @ Success(_) => (monad.pure(x), self2 || that.variant[A, Validation[E, B]])
        })
      }
    }
  }

  case class HigherOrderPimpM[M[_], A, B, C, D](value: ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])]) extends NewType[ActorM[M, (A, ActorM[M, C, D]), (B, ActorM[M, C, D])]] {
    /** Joins a higher order actor with its dependency to produce a lower 
     * order actor.
     */
    def <~ (that: ActorM[M, C, D])(implicit monad: Monad[M]): ActorM[M, A, B] = {
      receive { a: A =>
        val (mv, self2) = value ! ((a, that))

        unwrapM(mv.map {
          case ((b, that2)) =>
            (monad.pure(b), self2 <~ that2)
        })
      }
    }
  }

  case class ActorMStatePimp[M[_], A, B](value: ActorMState[M, A, B]) extends NewType[ActorMState[M, A, B]] {
    def premap[AA](f: AA => A): ActorMState[M, AA, B] = value.mapElements[M[B], ActorM[M, AA, B]](identity, _.premap[AA](f))
    
    def postmap[BB](f: B => BB)(implicit monad: Monad[M]): ActorMState[M, A, BB] = 
      value.mapElements[M[BB], ActorM[M, A, BB]](m => monad.fmap(m, f), _.postmap[BB](f))

    def variant[AA <: A, BB >: B](implicit monad: Monad[M]): ActorMState[M, AA, BB] = premap[AA](aa => (aa: A)).postmap[BB](b => (b: BB))
  }
}
object ActorMPimps extends ActorMPimps