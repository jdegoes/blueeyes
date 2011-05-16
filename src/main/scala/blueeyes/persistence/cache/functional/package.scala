package blueeyes.persistence.cache
package object functional {
  type NanoTime = Long

  type StageNext[K, V] = (Map[K, V], Stage[K, V])
}