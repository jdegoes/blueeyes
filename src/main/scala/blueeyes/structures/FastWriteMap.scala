package blueeyes.structures

/* Nathan Gerhart
 * 
 * History:
 **********
 * 08.30.11: NKG
 *  - Initial commit
 *  - removed +V and extension of Map class to get compilation working
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

// TODO: implement extension of Map class and covariant Value types
class FastWriteMap[K, V] (
    val mapData: MapData[K, V] = new MapData,
    val version: Int = 0) {
	
	// when modifying the FastWriteMap
	// check to make sure we have the right version
	// modify the underlying data
	// create and return a new fastWriteMap
	def + (kv: (K,V)): FastWriteMap[K,V] = {
	  val newMapData = mapData rollBackToVersion version
	  mapData += kv
	  return new FastWriteMap[K, V](newMapData, newMapData.version)
	}
	
	def - (key: K): FastWriteMap[K,V] = {
	  val newMapData = mapData rollBackToVersion version
	  mapData -= key
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
