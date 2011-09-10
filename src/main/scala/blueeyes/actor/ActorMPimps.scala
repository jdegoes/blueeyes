package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

trait ActorMPimps {
  import ActorMHelpers._
  import ActorMTypeclasses._

  implicit def ActorMPimpToMAB[M[_], A, B](pimp: ActorMPimp[M, A, B]) = ActorMToMAB(pimp.value)

  implicit def ToActorMPimp[M[_], A, B](actor: ActorM[M, A, B]) = ActorMPimp(actor)

  implicit def ToActorMStatePimp[M[_], A, B](value: ActorMState[M, A, B]): ActorMStatePimp[M, A, B] = ActorMStatePimp(value)

  sealed case class ActorMPimp[M[_], A, B](value: ActorM[M, A, B]) extends NewType[ActorM[M, A, B]] {
    def premap[AA](f: AA => A): ActorM[M, AA, B] = receive { aa: AA =>
      (value ! f(aa)).mapElements(identity, _.premap(f))
    }

     def -<- [AA](f: AA => A): ActorM[M, AA, B] = premap(f)

    def postmap[BB](f: B => BB)(implicit monad: Monad[M]): ActorM[M, A, BB] = value.map(f)

    def ->- [BB](f: B => BB)(implicit monad: Monad[M]): ActorM[M, A, BB] = value.map(f)

    def bimap[AA, BB](f: AA => A, g: B => BB)(implicit monad: Monad[M]): ActorM[M, AA, BB] = premap(f).postmap(g)

    def filter[Z[_], ZA](f: Z[ZA] => Boolean)(implicit empty: Empty[Z], witness: B => Z[ZA], functor: Functor[M]): ActorM[M, A, Z[ZA]] = receive { a: A =>
      val (b, next) = value ! a

      val mzza = functor.fmap(b, { b: B =>
          val zza: Z[ZA] = witness(b)

          if (f(zza)) zza else empty.empty[ZA]
        }
      )

      (mzza, next.filter(f))
    }

    def & [AA >: A, C](that: ActorM[M, AA, C])(implicit monad: Monad[M]): ActorM[M, A, (B, C)] = receive { a: A =>
      val (bM, selfNext) = value ! a
      val (cM, thatNext) = that ! a

      (for (b <- bM; c <- cM) yield (b, c), selfNext & thatNext)
    }

    def variant[AA <: A, BB >: B](implicit monad: Monad[M]): ActorM[M, AA, BB] = premap[AA](aa => (aa: A)).postmap[BB](b => (b: BB))

    def | [E, BB, AA >: A, BBB <: BB](that: ActorM[M, AA, Validation[E, BBB]])
        (implicit witness: B => Validation[E, BB], monad: Monad[M]): ActorM[M, A, Validation[E, BB]] = {
      receive { a: A =>
        val (result, next) = value.map(witness) ! a

        unwrapM(result.map { 
          case Failure(_) => (that ! a).variant[A, Validation[E, BB]].mapElements(identity, next | _)

          case x => (monad.pure(x), next | that.variant[A, Validation[E, BB]])
        })
      }
    }

    /** Same as "|" but sends both actors a message immediately. If in the Future monad,
     * this has the effect of parallelizing both responses and preferentially choosing 
     * the successful result of the first actor, but defaulting to the result of the 
     * second actor if the first actor fails.
     */
    def || [E, BB, AA >: A, BBB <: BB](that: ActorM[M, AA, Validation[E, BBB]])
        (implicit witness: B => Validation[E, BB], monad: Monad[M]): ActorM[M, A, Validation[E, BB]] = {
      receive { a: A =>
        val (result1, next1) = value.map(witness) ! a
        val (result2, next2) = (that ! a).variant[A, Validation[E, BB]]

        unwrapM(result1.map { 
          case Failure(_) => (result2, next1 || next2)

          case x => (monad.pure(x), next1 || that.variant[A, Validation[E, BB]])
        })
      }
    }

    def ^ [C, D] (that: ActorM[M, C, D])(implicit functor: Functor[M]): ActorM[M, Either[A, C], Either[B, D]] = {
      receive { 
        case Left(a)  => (value ! a).mapElements(_.map(Left.apply  _), _ ^ that)
        case Right(c) => (that  ! c).mapElements(_.map(Right.apply _), value ^ _)
      }
    }

    def <~ [AA, BB, C, D] (that: ActorM[M, C, D])(implicit monad: Monad[M], w1: Tuple2[AA, ActorM[M, C, D]] => A, w2: B => (BB, ActorM[M, C, D])): ActorM[M, AA, BB] = {
      val self = bimap(w1, w2)

      receive { aa: AA =>
        val (mv, self2) = self ! ((aa, that))

        unwrapM(mv.map {
          case (bb, that2) => (monad.pure(bb), self2 <~ that2)
        })
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

    def >- [C, CC, D](f: (C, CC) => D)(implicit witness: B => (C, CC), monad: Monad[M]): ActorM[M, A, D] = receive { a: A =>
      val (bM, next) = value ! a

      val d = for (b <- bM; val (c, cc) = witness(b)) yield f(c, cc)

      (d, next >- (f))
    }

    def merge[C, CC, D](f: (C, CC) => D)(implicit witness: B => (C, CC), monad: Monad[M]): ActorM[M, A, D] = this >- f

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

            (c, reduce((p1, a2), defaultCase))
          }
          else defaultCase ! b
        }
      }

      value >>> (cases.foldRight[ActorM[M, B, C]](defaultCase)(reduce))
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