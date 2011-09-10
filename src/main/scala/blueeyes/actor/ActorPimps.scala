package blueeyes.actor

import scalaz._
import scalaz.Scalaz._

import blueeyes.concurrent.Future

trait ActorPimps {
  import ActorHelpers._
  import ActorTypeclasses._

  implicit def ActorPimpToMAB[A, B](pimp: ActorPimp[A, B]) = ActorToMAB(pimp.value)

  implicit def ToActorPimp[A, B](actor: Actor[A, B]) = ActorPimp(actor)

  implicit def ActorStateToActorStatePimp[A, B](value: ActorState[A, B]): ActorStatePimp[A, B] = ActorStatePimp(value)

  sealed case class ActorPimp[A, B](value: Actor[A, B]) extends NewType[Actor[A, B]] {
    /** Maps the input values sent to the actor.
     */
    def premap[AA](f: AA => A): Actor[AA, B] = receive[AA, B] { aa: AA =>
      (value ! f(aa)).mapElements[B, Actor[AA, B]](identity, _.premap[AA](f))
    }

    /** 
     * @see premap
     */
    def -<- [AA](f: AA => A): Actor[AA, B] = premap(f)

    /** Maps the output values produced by the 
     */
    def postmap[BB](f: B => BB): Actor[A, BB] = value.map(f)

    /** 
     * @see postmap
     */
    def ->- [BB](f: B => BB): Actor[A, BB] = value.map(f)

    /** Maps both input and output values in one step.
     */
    def bimap[AA, BB](f: AA => A, g: B => BB): Actor[AA, BB] = premap(f).postmap(g)

    /** Filters out all values not accepted by the specified predicate. 
     */
    def filter[Z[_], ZA](f: B => Boolean)(implicit empty: Empty[Z], witness: B => Z[ZA]): Actor[A, Z[ZA]] = receive { a: A =>
      val (b, next) = value ! a

      val z = witness(b)

      (if (f(b)) z else implicitly[Empty[Z]].empty[ZA], next.filter(f))
    }

    /** Returns a new actor, which given an input value, produces a tuple 
     * holding the outputs for this actor AND the specified 
     */
    def & [AA >: A, C](that: Actor[AA, C]): Actor[A, (B, C)] = receive { a: A =>
      val (b, selfNext) = value ! a
      val (c, thatNext) = that ! a

      ((b, c), selfNext & thatNext)
    }

    /** Returns a new actor, which given an input value, produces an output
     * value from this actor OR the specified actor -- whichever is the first
     * to produce a non-failure output.
     *
     * This function also considers a match error to be a failure, so it may be
     * safely used with actors defined by partial functions. However, the 
     * recommended approach is to form the actor with receiveSome, passing
     * a partial function.
     */
    def | [E, BB, AA >: A, BBB <: BB](that: Actor[AA, Validation[E, BBB]])(implicit witness: B => Validation[E, BB]): Actor[A, Validation[E, BB]] = {
      receive { a: A =>
        try {
          val result = (value.map(witness) ! a)

          result.mapElements(identity, _ | that.variant[A, Validation[E, BB]]) match {
            case (Failure(_), next) => (that ! a).variant[A, Validation[E, BB]].mapElements(identity, next | _)

            case x => x
          }
        }
        catch {
          case e: MatchError => (that ! a).variant[A, Validation[E, BB]].mapElements(identity, this | _)
        }
      }
    }

    /** Accepts one or the other input type and produces the corresponding 
     * output type.
     */
    def ^ [C, D] (that: Actor[C, D]): Actor[Either[A, C], Either[B, D]] = {
      receive { 
        case Left(a)  => (value ! a).mapElements(Left.apply  _, _ ^ that)
        case Right(c) => (that  ! c).mapElements(Right.apply _, value ^ _)
      }
    }

    /** Joins a higher order actor with its dependency to produce a lower 
     * order actor.
     */
    def <~ [AA, BB, C, D] (that: Actor[C, D])(implicit w1: Tuple2[AA, Actor[C, D]] => A, w2: B => (BB, Actor[C, D])): Actor[AA, BB] = {
      val self = bimap(w1, w2)

      receive { aa: AA =>
        val ((bb, that2), self2) = self ! ((aa, that))

        (bb, self2 <~ that2)
      }
    }

    /** Joins a higher order actor with its dependency to produce a lower 
     * order actor.
     */
    def ~> [C, D] (that: Actor[(C, Actor[A, B]), (D, Actor[A, B])]): Actor[C, D] = {
      receive { c: C =>
        val ((d, this2), that2) = that ! ((c, value))

        (d, this2 ~> that2)
      }
    }

    /** Returns a new actor, which accepts a tuple and produces a tuple, which 
     * are formed from the input and output values for this actor and the 
     * specified actor, respectively. This operation is called "cross".
     */
    def * [AA, BB](that: Actor[AA, BB]): Actor[(A, AA), (B, BB)] = receive {
      case (a, aa) =>
        val (b,  next1) = value ! a
        val (bb, next2) = that ! aa

        ((b, bb), next1 * next2)
    }

    /** If this actor produces a tuple, you can use this function to merge the
     * elements of the tuple into a single value.
     */
    def >- [C, CC, D](f: (C, CC) => D)(implicit witness: B => (C, CC)): Actor[A, D] = receive { a: A =>
      val (b, next) = value ! a

      val (c, cc) = witness(b)
       
      val d = f(c, cc)

      (d, next >- (f))
    }

    /** @see >-
     */
    def merge[C, CC, D](f: (C, CC) => D)(implicit witness: B => (C, CC)): Actor[A, D] = this >- f

    /** Splits the output into a tuple of the output.
     */
    def split: Actor[A, (B, B)] = {
      lazy val lazySelf: Actor[A, (B, B)] = receive { a: A =>
        val (b, next) = value ! a

        ((b, b), lazySelf)
      }

      lazySelf
    }

    /** Classic scan ("fold" with the output being the intermediate values).
     */
    def scan[Z](z: Z)(f: (Z, B) => Z): Actor[A, Z] = receive { a: A =>
      val (b, next) = value ! a

      val z2 = f(z, b)

      (z2, next.scan(z2)(f))
    }

    /** Folds over the output values generated by the specified sequence of 
     * input values, to generate a single final result, together with the
     * continuation of this actor.
     */
    def fold[Z](z: Z, as: Seq[A])(f: (Z, B) => Z): (Z, Actor[A, B]) = as.foldLeft[(Z, Actor[A, B])]((z, value)) {
      case ((z, actor), a) =>
        val (b, actor2) = actor ! a

        (f(z, b), actor2)
    }

    /** Switches between two actors based on a boolean predicate.
     * {{{
     * a.ifTrue(_ % 2 == 0)(then = multiply, orELse = divide)
     * }}}
     */
    def ifTrue[C](f: B => Boolean)(then: Actor[B, C], orElse: Actor[B, C]): Actor[A, C] = switch(orElse)(f -> then)

    /** Switches between multiple actors based on boolean predicates.
     */
    def switch[C](defaultCase: Actor[B, C])(cases: (B => Boolean, Actor[B, C])*): Actor[A, C] = {
      def reduce(t: (B => Boolean, Actor[B, C]), orElse: Actor[B, C]): Actor[B, C] = {
        val (p1, a1) = t

        receive { (b: B) =>
          if (p1(b)) {
            val (c, a2) = a1 ! b

            (c, reduce((p1, a2), defaultCase))
          }
          else defaultCase ! b
        }
      }

      value >>> (cases.foldRight[Actor[B, C]](defaultCase)(reduce))
    }

    /** Lifts the output values into a monad.
     *
     * Note: Don't use this to create an asynchronous actor (using the Future 
     * monad), or the resulting actor will block! Blame strictness.
     */
    def lift[M[_]](implicit monad: Monad[M]): ActorM[M, A, B] = ActorMHelpers.receive { a: A =>
      val (b, next) = value ! a

      (monad.pure(b), next.lift[M])
    }

    /** Converts a synchronous actor into an aynchronous actor.
     */
    def async: ActorAsync[A, B] = {
      def unwrap[A, B](future: Future[ActorState[A, B]]): ActorStateAsync[A, B] = {
        (future.map(_._1), ActorMHelpers.receive[Future, A, B] { a: A =>
          unwrap(future.map {
            case (_, next) => next ! a
          })
        })
      }

      ActorMHelpers.receive[Future, A, B] { a: A =>
        unwrap(Future.async(value ! a))
      }
    }

    /** Workaround invariance of actor.
     */
    def variant[AA <: A, BB >: B]: Actor[AA, BB] = premap[AA](aa => (aa: A)).postmap[BB](b => (b: BB))
  }

  case class ActorStatePimp[A, B](value: ActorState[A, B]) extends NewType[ActorState[A, B]] {
    def premap[AA](f: AA => A): (B, Actor[AA, B]) = value.mapElements[B, Actor[AA, B]](identity, _.premap[AA](f))
    
    def postmap[BB](f: B => BB): (BB, Actor[A, BB]) = value.mapElements[BB, Actor[A, BB]](f, _.postmap[BB](f))

    def variant[AA <: A, BB >: B]: (BB, Actor[AA, BB]) = value.mapElements((b: B) => (b : BB), _.variant[AA, BB])
  }
}
object ActorPimps extends ActorPimps