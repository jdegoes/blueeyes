package blueeyes.persistence.cache

import scala.collection.mutable.Map
import java.util.{Map => JavaMap, LinkedHashMap}
import java.util.concurrent.{ConcurrentMap => JavaConcurrentMap}

private[cache] class LRUMap[K, V](evicter: (K, V) => Unit, maximumWeightedCapacity: Int, weigh: V => Int) extends LinkedHashMap[K, V] with JavaConcurrentMap[K, V] {
  private var totalWeight: Int = 0

  private def evict(key: K, value: V) = {
    try {
      evicter(key, value)
    }
    catch {
      case e => e.printStackTrace
    }
  
    decrementTotalWeight(value)
  }
  
  private def decrementTotalWeight(value: V) = totalWeight -= weigh(value)
  private def incrementTotalWeight(value: V) = totalWeight += weigh(value)

  override protected def removeEldestEntry(entry: JavaMap.Entry[K, V]): Boolean = {
    val key   = entry.getKey
    val value = entry.getValue
    
    val remove = totalWeight > maximumWeightedCapacity
    
    if (remove) {
      try {
        evict(key, value)
      }
      catch {
        case e => e.printStackTrace
      }
    }
    
    remove
  }
  
  override def put(key: K, newValue: V): V = {
    incrementTotalWeight(newValue)
    
    super.put(key, newValue)
  }
  
  override def putAll(that: JavaMap[K2, V2] forSome { type K2 <: K; type V2 <: V}) = {
    foreach(that.entrySet) { entry =>
      put(entry.getKey, entry.getValue)
    }
  }
  
  override def remove(key: Any): V = super.remove(key) match {
    case value if (value != null) => 
      decrementTotalWeight(value)
    
      value
      
    case any => any
  }
  
  override def clear() = {
    foreach(keySet) { key =>
      remove(key)
    }
    
    totalWeight = 0
    
    super.clear()
  }
  
  def putIfAbsent(key: K, value: V): V = {
    get(key) match {
      case null => 
        put(key, value)
        
        value
      
      case v => v 
    }
  }
  
  def remove(key: AnyRef, value: AnyRef): Boolean = {
    get(key) match {
      case null => false
      
      case `value` => 
        remove(key); 
        
        true
      
      case _ => false
    }
  }
  
  def replace(key: K, value: V): V = {
    if (containsKey(key)) put(key, value)
    else null.asInstanceOf[V]
  }
  
  def replace(key: K, oldValue: V, newValue: V): Boolean = {
    get(key) match {
      case `oldValue` => 
        put(key, newValue)
        
        true
        
      case _ => false
    }
  }
  
  private def foreach[S](c: java.util.Collection[S])(f: S => Unit) {
    val iterator = c.iterator
    
    while (iterator.hasNext) { f(iterator.next) }
  }
}
