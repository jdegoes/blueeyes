package blueeyes.health

import collection.mutable.ConcurrentMap

private[health] object ConcurrentMaps{
  def createIfAbsent[K, V](key: K, container: ConcurrentMap[K, V], factory: => V): V = {
    container.get(key).getOrElse({
      val statObject = factory
      container.putIfAbsent(key, statObject).getOrElse(statObject)
    })
  }
}