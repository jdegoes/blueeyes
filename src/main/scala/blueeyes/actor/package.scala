package blueeyes

import blueeyes.concurrent.Future

package object actor {
  type ActorState[A, B] = (B, Actor[A, B])

  type ActorMState[M[_], A, B] = M[(B, ActorM[M, A, B])]

  type ActorAsync     [A, B] = ActorM     [Future, A, B]
  type ActorStateAsync[A, B] = ActorMState[Future, A, B]
}