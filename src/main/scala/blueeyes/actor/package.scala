package blueeyes

import akka.dispatch.Future

package object actor {
  type ActorState[A, B] = (B, Actor[A, B])

  type ActorTState[M[_], A, B] = M[(B, ActorT[M, A, B])]

  type ActorAsync     [A, B] = ActorT     [Future, A, B]
  type ActorStateAsync[A, B] = ActorTState[Future, A, B]
}