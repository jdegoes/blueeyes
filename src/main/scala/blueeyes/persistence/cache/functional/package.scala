package blueeyes.persistence.cache

import blueeyes.actor._

package object functional {
  type NanoTime = Long

  type StageActor[K, V] = Actor     [StageIn[K, V], Map[K, V]]
  type StageState[K, V] = ActorState[StageIn[K, V], Map[K, V]]
}