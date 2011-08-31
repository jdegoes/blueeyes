package blueeyes.structures

import scala.collection.mutable.{Map, HashMap}
import blueeyes.concurrent.ReadWriteLock

/* Nathan Gerhart
 * 
 * History:
 **********
 * 08.30.11: Initial commit
 * 
 * Docs:
 * *********
 * This is a mutable map with an undoable history designed for use
 *  under-the-hood with the FastWriteMap class.  The UndoableOperations are
 *  meant to only work with the MutableMap class, so they are included here
 *  as well. 
 */

// need a base mutable map, so use a hashmap
class MutableMap [K, +V] extends HashMap [K,V] {
  
}
  
// the operations must take a map as an argument, because we need to
// execute the operation on one map, and then undo it on a copy of that map
abstract class UndoableOperation [K, V] {
  def executeOn (map: MutableMap[K, V]): Unit
  def undoOn (map: MutableMap[K, V]): Unit
}

class AddOperation [K, V] (key: K, newVal: V) extends UndoableOperation [K, V] {
  
  var oldVal: Option[V] = None
  
  def executeOn (map: MutableMap[K,V]) = {
    // keep track of the old value in case we are overwriting
    oldVal = map get key
    map += (key -> newVal)
  }
  
  def undoOn (map: MutableMap[K,V]) = {
    if (oldVal == None) {
      // used add to add a new value, so just remove the key
      map -= key
    } else {
      // used add to overwrite an old value, so reset the value
      map += (key -> oldVal.get)
    }
  }
}

class RemoveOperation [K, V] (key: K) extends UndoableOperation [K,V] {
  
  var oldVal: Option[V] = None
  
  def executeOn (map: MutableMap[K,V]) = {
    // keep track of the old value for undoing
    oldVal = map get key
    map -= key
  }
  
  def undoOn (map: MutableMap[K,V]) = {
    if (oldVal != None) {
      // put the old value back in the map
      map += (key -> oldVal.get)
    }
    // if there was no old value, don't do anything
  }
}

// the internal structure of the FastWriteMap class
case class MapData[K, +V] (
		map: MutableMap [K,V] = MutableMap(),
		var history: Vector[UndoableOperation[K, V]] = Vector())
	extends Map [K, V] with ReadWriteLock {

  // rather than keep checking the length of the history, just keep a version count here too
  var version: Int = 0
  
  // when adding items to the MapData
  // create an add-operation
  // append the operation on the history
  // execute the operation on the hashmap
  def += (kv: (K,V)): MapData = {
    val adder: AddOperation[K,V] = AddOperation[K,V](kv(0), kv(1))
    writeLock ({
      history = history :+ adder
      version += 1
      adder executeOn map
    })
  }
  
  // when removing items from the hash table
  // create a remove-operation
  // append the operation on the history
  // execute the operation on the map
  def -= (key: K): MapData = {
	val remover: RemoveOperation[K,V] = RemoveOperation[K,V](key)
	writeLock ({
	  history = history :+ remover
	  version += 1
	  remover executeOn map
	})
  }
  
  def get (key: K): Option[V] = {
    readLock (map.get(key))
  }
  
  def iterator: Iterator[(K,V)] = {
    readLock(map.iterator)
  }
  
  // when accessing an old version of the FastWriteMap, we need to undo
  //  the MapData back to the correct version and go from there
  // This function rolls back appropriately
  def rollBackToVersion (newVersion: Int): MapData = {
    if (version == newVersion) {
      // just return the original object
      map
      } else if (version < newVersion) {
        readLock (this.copy(history.slice(version, newVersion)))
        } else {
          throw new Error("Can't roll back to the future!")
          }
    }
   
  // to create a copy of the mapData at an earlier point in time,
  // take the mapData object and the vector of items to undo
  // make a copy of the map (hashmap)
  // undo items one by one until there are no more
  // return a new mapData object
  private def copy (undoQueue: Vector[UndoableOperation[K, V]]): MapData = {
    val newMap: MutableMap = map.clone()
    for (operation <- undoQueue.reverse) {
      operation undoOn newMap
    }
    return new MapData(newMap)
  }
  
}