package blueeyes.structures

import org.specs.Specification

/* Nathan Gerhart
 * 
 * History:
 **********
 * 09.07.11: NKG
 *  - Initial commit
 * 
 * Docs:
 * *********
 * The tests here are highly dependent on being executed sequentially with
 *  side effects shared between the different tests.
 */

class FastWriteMapSpec extends Specification {
  
  "FastWriteMap" should { 
    var fastMap = new FastWriteMap[Int,String]
    fastMap += (1 -> "a")
    fastMap += (2 -> "b")
    fastMap += (3 -> "c")
    shareVariables()
    "share underlying MapData structure" in { 
      val newMap = fastMap + (4 -> "d")
      newMap must notEq(fastMap)
      newMap.mapData mustEq fastMap.mapData
      newMap.get(4).get must_== "d"
      fastMap.get(4) must_== None // the immutable object isn't modified
      fastMap.mapData.get(4).get must_== "d" // but the underlying object is
    }
    "be immutable with" in { 
      "kv adding" in { 
	val newMap = fastMap + (4 -> "d")
	newMap must notEq(fastMap) // they are different objects
	fastMap.get(4) must_== None
      }
      "kv overwriting" in { 
	val newMap = fastMap + (2 -> "B")
	fastMap.get(2).get must_== "b"
	newMap.get(2).get must_== "B"
      }
      "kv removing" in { 
	val newMap = fastMap - 2
	fastMap.get(2).get must_== "b"
	newMap.get(2) must_== None
      }
    }
    "handle long histories" in { 
      val oldMax = MapData.MAX_HISTORY
      MapData.MAX_HISTORY = 3
      var bigMap = new FastWriteMap[Int,String]
      bigMap += (1 -> "a")
      bigMap += (2 -> "b")
      bigMap += (3 -> "c")
      val newMap = bigMap + (4 -> "d")
      bigMap.mapData must notEq(newMap.mapData) // reset the history
      bigMap.version must_== 3
      newMap.version must_== 1
      MapData.MAX_HISTORY = oldMax // reset the history length
    }
    "handle addition of loose-typed values" in { 
      val oldMap = fastMap + (1 -> "a") // fastMap is out of date, so need to start a branch to get the underlying data to be shared
      val newMap = oldMap + (1 -> 1.0)
      newMap.mapData mustEq oldMap.mapData
      newMap.get(1).get must_== 1.0
      oldMap.get(1).get must_== "a"
      oldMap.mapData.get(1).get must throwAn[java.lang.ClassCastException] // type mismatch (expect string, get float)
    }
  }
}
