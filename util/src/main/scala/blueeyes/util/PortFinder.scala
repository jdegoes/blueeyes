package blueeyes.util

import java.net.{InetSocketAddress, ServerSocket}

import scala.util.Random

import scalaz.syntax.std.boolean._

trait PortFinder {
  /** The minimum random port number that can be used by the underlying mongo instance */
  def portRangeStart = 50000

  /** The size of the random port range to be used by the underlying mongo instance */
  def portRangeSize  = 10000

  // Shamlessly borrowed from Camel:
  // http://svn.apache.org/viewvc/camel/trunk/components/camel-test/src/main/java/org/apache/camel/test/AvailablePortFinder.java?view=markup#l130
  protected def isAvailable(port: Int): Boolean = {
    var ss: ServerSocket = null
    try {
      ss = new ServerSocket();
      ss.setReuseAddress(true);
      ss.bind(new InetSocketAddress(port))
      true
    } catch { case t: Exception =>
      false
    } finally {
      if (ss != null) {
        try {
          ss.close();
        } catch {
          case t => t.printStackTrace
        }
      }
    }
  }

  protected def findUnusedPort(tries: Int): Option[Int] = findUnusedPorts(tries, 1).map(_.head)

  protected def findUnusedPorts(tries: Int, count: Int): Option[List[Int]] = {
    def randomPortStream: Stream[Int] = (Random.nextInt(portRangeSize) + portRangeStart) #:: randomPortStream
    val result = randomPortStream.take(tries).filter(isAvailable).take(count)

    (result.size == count).option(result.toList)
  }
}
