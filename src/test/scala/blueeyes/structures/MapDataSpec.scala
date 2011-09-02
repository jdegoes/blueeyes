package blueeyes.structures

import org.specs.Specification
import scala.collection.mutable.HashMap

/* Nathan Gerhart
 * 
 * History:
 **********
 * 08.30.11: NKG
 *  - Initial commit
 * 08.31.11: NKG
 *  - Formatted
 *  - Added test to show reset of history exceeding MAX_HISTORY
 * 09.01.11: NKG
 *  - Added test to show addition of looser-typed values
 *  - Updated undo test to be more certain it works
 * 
 * Docs:
 * *********
 * The tests here are highly dependent on being executed sequentially with
 *  side effects shared between the different tests.  Importantly, this glosses
 *  over the fact that Undoable Operations must be executed before they are
 *  undone. 
 */

class MapDataSpec extends Specification {

  "Undoable Operations" should {
    val mutableMap1 = new HashMap[String, String]
    var mutableMap2 = mutableMap1.clone
    val addOp1 = new AddOperation("1", "a")
    val addOp2 = new AddOperation("2", "B")
    val overOp = new AddOperation("2", "b")
    val remOp1 = new RemoveOperation[String, String]("1")
    shareVariables()
    "be executable on a hashmap" in {
      "with adding kv pair" in {
        mutableMap1.get("1") must_== None
        addOp1 executeOn mutableMap1
        mutableMap1.get("1") must_== Some("a")
      }
      "with overwriting kv pair" in {
        addOp2 executeOn mutableMap1
        mutableMap1.get("2") must_== Some("B")
        overOp executeOn mutableMap1
        mutableMap1.get("2") must_== Some("b")
      }
      "with removing key" in {
        remOp1 executeOn mutableMap1
        mutableMap1.get("1") must_== None
      }
    }
    "be undoable on a different hashmap" in {
      mutableMap2 = mutableMap1.clone()
      "kv remove" in {
        mutableMap2.get("1") must_== None
        remOp1 undoOn mutableMap2
        mutableMap2.get("1") must_== Option("a")
      }
      "kv overwrite" in {
        mutableMap2.get("2") must_== Option("b")
        overOp undoOn mutableMap2
        mutableMap2.get("2") must_== Option("B")
      }
      "kv add" in {
        mutableMap2.get("2") must_== Option("B")
        addOp2 undoOn mutableMap2
        mutableMap2.get("2") must_== None
        addOp1 undoOn mutableMap2
      }
    }
  }

  "MapData" should {
    val map = new MapData[String, String]
    shareVariables()
    "be mutable with" in {
      "adding kv pairs" in {
        map.version must_== 0
        map.history.length must_== 0
        // add a pair
        map += ("1" -> "a")
        map.get("1") must_== Option("a")
        map.version must_== 1
        map.history.length must_== 1
        // add another pair
        map += ("2" -> "B")
        map.get("2") must_== Option("B")
        map.version must_== 2
        map.history.length must_== 2
      }
      "overwriting values" in {
        // overwrite a pair
        map += ("2" -> "b")
        map.get("2") must_== Option("b")
        map.version must_== 3
        map.history.length must_== 3
      }
      "removing kv pairs" in {
        map -= "1"
        map.get("1") must_== None
        map.version must_== 4
        map.history.length must_== 4
      }
    }
    "handle variable value types" in { 
      val newMap = map + ("3" -> 'c) // not a String, so newMap has looser type
      newMap.get("2").get must_== "b"
      newMap.get("3").get must_== 'c
      newMap.version must_== 1
      // doesn't modify the existing object
      map.get("3") must_== None
      val anyMap = newMap + ("4" -> 4)
      anyMap.get("4").get must_== 4
      newMap.get("4") must_== None
    }
    "be forkable with" in {
      "a complete reset" in {
        val newMap = map.rollBackToVersion(0)
        newMap.get("1") must_== None
	newMap.get("2") must_== None
        newMap.version must_== 0
        newMap.history.length must_== 0
      }
      "a partial rollback" in {
        val newMap = map.rollBackToVersion(2)
        newMap.get("1") must_== Some("a")
        newMap.get("2") must_== Some("B")
        newMap.version must_== 0
        newMap.history.length must_== 0
      }
      "no rollback when history is too long" in {
        val bigMap = new MapData[String, String]
        val oldMax = MapData.MAX_HISTORY
        MapData.MAX_HISTORY = 3
        bigMap += ("1" -> "a")
        bigMap += ("2" -> "b")
        var newMap = bigMap rollBackToVersion 2
        newMap mustEq bigMap // they are the same object
        bigMap += ("3" -> "c")
        newMap = bigMap rollBackToVersion 3
        newMap must notEq(bigMap) // now we've forked things
        newMap.history.length must_== 0
        newMap.version must_== 0
	// reset the MAX_HISTORY
	MapData.MAX_HISTORY = oldMax
      }
    }
    "not be forkable with no rollback" in {
      val newMap = map.rollBackToVersion(4)
      newMap mustEq map // they are the same object!
    }
    "be copiable" in {
      val newMap = map.copy(Vector[UndoableOperation[String, String]]())
      newMap must notEq(map) // they are different objects
      // with the same key/vals
      map.get("1") must_== newMap.get("1")
      map.get("2") must_== newMap.get("2")
      // but the copy has no history
      newMap.history.length must_== 0
      newMap.version must_== 0
    }
  }

}
