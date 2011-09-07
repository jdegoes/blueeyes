package blueeyes.structures

import scala.collection.immutable.Map

/* Nathan Gerhart
 * 
 * History:
 **********
 * 08.30.11: NKG
 *  - Initial commit
 *  - removed +V and extension of Map class to get compilation working
 * 09.07.11: NKG
 *  - updated to be an extension of immutable.Map
 *  - modified + method to cast mapData to appropriate type
 *  - modified - method to modify the rolled-back mapData
 * 
 * Docs:
 * *********
 * This is an immutable map with a mutable map (MapData) under-the-hood.
 *  Each operation (+/-) returns a new instance of the FastWriteMap while
 *  modifying the underlying MapData instance.  Accessing an older copy of
 *  a map rebuilds a new MapData, which is built by taking the old mutable
 *  map, and undoing all later operations to arrive at the earlier state of
 *  the map.
 */

class FastWriteMap[K, V] (
    val mapData: MapData[K, V] = new MapData[K, V],
    val version: Int = 0) extends Map[K, V] {
	
	// when modifying the FastWriteMap
	// check to make sure we have the right version and type
	// modify the underlying data
	// create and return a new fastWriteMap
	def + [V1 >: V](kv: (K,V1)): FastWriteMap[K,V1] = {
	  val newMapData: MapData[K,V1]= mapData.rollBackToVersion(version).asInstanceOf[MapData[K,V1]]
	  newMapData += kv
	  return new FastWriteMap[K, V1](newMapData, newMapData.version)
	}
	
	def - (key: K): FastWriteMap[K,V] = {
	  val newMapData = mapData rollBackToVersion version
	  newMapData -= key
	  return new FastWriteMap[K, V](newMapData, newMapData.version)
	}
	
	// if there are lots of calls to get on an old version, this will be
	// super slow, because we roll back the internal object over and over
	// but it doesn't make sense to modify the FastWriteMap due to the
	// pure interface
	def get (key: K): Option[V] = {
	  mapData.rollBackToVersion(version) get key
	}
	
	// same thing here as with get
	def iterator: Iterator[(K,V)] = {
	  mapData.rollBackToVersion(version).iterator
	}
	
	/* TODO: ++ and -- methods will be super slow because they probably just
	* call the +/- methods over and over.  Ideally, we could override the
	* ++ and -- methods at the MapData level and expose them here just like
	* the existing methods
	*/ 
}
