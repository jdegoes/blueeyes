package blueeyes.core.service

import akka.dispatch.Future
import akka.dispatch.Await
import akka.util._

import blueeyes.bkka._
import blueeyes.core.http._
import blueeyes.core.data._
import blueeyes.util.metrics.DataSize
import DefaultBijections._
import DataSize._

import java.nio.ByteBuffer

import blueeyes.core.http.test.HttpRequestMatchers
import org.specs2.mutable.Specification

import scalaz._
import scalaz.syntax.monad._

class HttpClientByteChunkSpec extends Specification with TestAkkaDefaults with HttpRequestMatchers { test =>
  def stream = Array[Byte]('1', '2') :: Array[Byte]('3', '4') :: StreamT.empty[Future, Array[Byte]]

  "HttpClientByteChunk" should {
    "aggregate full content when size is not specified" in{
      val future = client(Right(stream)).aggregate(8192.bytes).get[String]("foo")

      future must succeedWithContent { 
        be_==("1234")
      }
    }

    "aggregate full content up to the specified size" in {
      val firstBuffer = client(Right(stream)).aggregate(2.bytes).get[ByteChunk]("foo").flatMap { 
        _.content match { case Some(Right(stream)) => stream.head }  
      }

      Await.result(firstBuffer, Duration(2L, "seconds")) must beLike {
        case bytes => bytes.toList must_== List[Byte]('1','2')
      }
    }
  }

  private def client(content: ByteChunk) = HttpClientImpl(content)

  case class HttpClientImpl(content: ByteChunk) extends HttpClientByteChunk {
    val executor = test.defaultFutureDispatch
    def isDefinedAt(request: HttpRequest[ByteChunk]) = true
    def apply(request: HttpRequest[ByteChunk]) = Future(HttpResponse[ByteChunk](content = Some(content)))
  }
}
