package blueeyes.core.service

import org.specs2.mutable.Specification
import akka.dispatch.Future
import blueeyes.core.http._
import blueeyes.core.http.test.HttpRequestMatchers
import blueeyes.core.data.{ByteMemoryChunk, ByteChunk}
import blueeyes.util.metrics.DataSize
import DataSize._

class HttpClientByteChunkSpec extends Specification with blueeyes.bkka.AkkaDefaults with HttpRequestMatchers {
  "HttpClientByteChunk" should {
    "aggregate full content when size is not specified" in{
      val future = client(new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future(new ByteMemoryChunk(Array[Byte]('3', '4')))))).aggregate(None).get("foo")

      future must succeedWithContent {
        (v: ByteChunk) => new String(v.data) must_== "1234"
      }
    }

    "aggregate content up to the specified size" in{
      val future = client(new ByteMemoryChunk(Array[Byte]('1', '2'), () => Some(Future(new ByteMemoryChunk(Array[Byte]('3', '4')))))).aggregate(Some(2.bytes)).get("foo")

      future must succeedWithContent {
        (v: ByteChunk) => new String(v.data) must_== "12"
      }
    }
  }

  private def client(content: ByteChunk) = HttpClientImpl(content)

  case class HttpClientImpl(content: ByteChunk) extends HttpClientByteChunk{
    def isDefinedAt(request: HttpRequest[ByteChunk]) = true
    def apply(request: HttpRequest[ByteChunk]) = Future(HttpResponse[ByteChunk](content = Some(content)))
  }
}
