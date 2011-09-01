package blueeyes.structures

import scala.collection.mutable.{Map, HashMap}
import blueeyes.concurrent.ReadWriteLock

/* Nathan Gerhart
 * 
 * History:
 **********
 * 08.30.11: NKG
 *  - Initial commit
 *  - removed MutableMap class and just use built-in HashMap
 *  - fixed method and constructor parameterization that eclipse didn't catch
 *  - fixed rollBackToVersion based on results of tests
 * 
 * Docs:
 * *********
 * This is a mutable map with an undoable history designed for use
 *  under-the-hood with the FastWriteMap class.  The UndoableOperations are
 *  meant to only work with the mutable HashMap class, so they are included here
 *  as well. 
 */

// the operations must take a map as an argument, because we need to
// execute the operation on one map, and then undo it on a copy of that map
abstract class UndoableOperation [K, V] {
  def executeOn (map: HashMap[K, V]): Unit
  def undoOn (map: HashMap[K, V]): Unit
}

class AddOperation [K, V] (key: K, newVal: V) extends UndoableOperation [K, V] {
  
  var oldVal: Option[V] = None
  
  def executeOn (map: HashMap[K,V]) = {
    // keep track of the old value in case we are overwriting
    oldVal = map get key
    map += (key -> newVal)
  }
  
  def undoOn (map: HashMap[K,V]) = {
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
  
  def executeOn (map: HashMap[K,V]) = {
    // keep track of the old value for undoing
    oldVal = map get key
    map -= key
  }
  
  def undoOn (map: HashMap[K,V]) = {
    if (oldVal != None) {
      // put the old value back in the map
      map += (key -> oldVal.get)
    }
    // if there was no old value, don't do anything
  }
}

// the internal structure of the FastWriteMap class
case class MapData[K, V] (
		mutableMap: HashMap [K,V] = new HashMap[K, V],
		var history: Vector[UndoableOperation[K, V]] = Vector())
	extends Map [K, V] with ReadWriteLock {

  // rather than keep checking the length of the history, just keep a version count here too
  var version: Int = 0
  
  // when adding items to the MapData
  // create an add-operation
  // append the operation on the history
  // execute the operation on the hashmap
  def += (kv: (K,V)): MapData.this.type = {
    val adder: AddOperation[K,V] = new AddOperation[K,V](kv._1, kv._2)
    writeLock ({
      history = history :+ adder
      version += 1
      adder executeOn mutableMap
    })
    return this
  }
  
  // when removing items from the hash table
  // create a remove-operation
  // append the operation on the history
  // execute the operation on the map
  def -= (key: K): MapData.this.type = {
	val remover: RemoveOperation[K,V] = new RemoveOperation[K,V](key)
	writeLock ({
	  history = history :+ remover
	  version += 1
	  remover executeOn mutableMap
	})
    return this
  }
  
  def get (key: K): Option[V] = {
    readLock (mutableMap.get(key))
  }
  
  def iterator: Iterator[(K,V)] = {
    readLock(mutableMap.iterator)
  }
  
  // when accessing an old version of the FastWriteMap, we need to undo
  //  the MapData back to the correct version and go from there
  // This function rolls back appropriately
  def rollBackToVersion (newVersion: Int): MapData[K,V] = {
    if (version == newVersion) {
      // just return the original object
     this
      } else if (newVersion < version) {
        readLock (this.copy(history.slice(newVersion, version)))
        } else {
          throw new Error("Can't roll back to the future!")
          }
    }
   
  // to create a copy of the mapData at an earlier point in time,
  // take the mapData object and the vector of items to undo
  // make a copy of the map (hashmap)
  // undo items one by one until there are no more
  // return a new mapData object
  def copy (undoQueue: Vector[UndoableOperation[K, V]]): MapData[K,V] = {
    val newMap: HashMap[K,V] = mutableMap.clone()
    for (operation <- undoQueue.reverse) {
      operation undoOn newMap
    }
    return new MapData(newMap)
  }
  
}
