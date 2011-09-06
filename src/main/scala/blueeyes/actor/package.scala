package blueeyes

import blueeyes.concurrent.Future

package object actor {
  type Actor[A, B] = ActorV[A, B]

  type ActorState[A, B] = (B, Actor[A, B])

  type ActorMState[M[_], A, B] = ActorState[A, M[B]]
  type ActorM     [M[_], A, B] = Actor     [A, M[B]]

  type ActorAsync     [A, B] = ActorM     [Future, A, B]
  type ActorStateAsync[A, B] = ActorMState[Future, A, B]
}