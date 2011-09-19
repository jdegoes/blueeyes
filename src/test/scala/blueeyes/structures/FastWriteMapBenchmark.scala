package blueeyes.structures

import scala.testing.Benchmark
import scala.collection.immutable.HashMap

// test the FastWriteMap with 10,000 adds
// (note that if you use "multiplier" then there will be overwrites as well)
object FastWriteMapBench extends Benchmark { 
  var map = new FastWriteMap[Int, Int]

  override def setUp = { 
    map = new FastWriteMap[Int,Int]
  }

  def run = { 
    for (i <- 1 to 10000) { 
      map = map + (i -> i)
    }
  }
}

// test the default implementation with 10,000 adds
object HashMapBench extends Benchmark { 
  var map = new HashMap[Int, Int]

  override def setUp = { 
    map = new HashMap[Int,Int]
  }

  def run = { 
    for (i <- 1 to 10000) { 
      map = map + (i -> i)
    }
  }
}

// test the FastWriteMap with 10,000 adds to an OUT-OF-DATE map
//  this should be super slow because we are constantly copying new maps
object SlowWriteMapBench extends Benchmark { 
  var map = new FastWriteMap[Int, Int]

  override def setUp = { 
    map = new FastWriteMap[Int,Int]
  }

  def run = { 
    for (i <- 1 to 10000) { 
      // no reassignment, so map is always "older"
      map + (i -> i)
    }
  }
}
