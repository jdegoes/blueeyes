package blueeyes.core.data

import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.Await
import akka.util.Duration

import blueeyes.bkka._
import blueeyes.akka_testing.FutureMatchers

import java.io.File
import java.nio.ByteBuffer

import org.specs2.mutable.Specification
import org.specs2.specification.{AfterExample, BeforeAfterExample}

import scalaz._
import scalaz.syntax.monad._
import scala.collection.mutable.ArrayBuilder.ofByte

trait Data extends TestAkkaDefaults {
  val dataFile = new File(System.getProperty("java.io.tmpdir") + File.separator + System.currentTimeMillis)
  val data     = List.fill(5)(List.fill[Byte](10)('0'))
  val chunk    = Right(StreamT.fromStream[Future, ByteBuffer](Future(data.map(lb => ByteBuffer.wrap(lb.toArray)).toStream)))
}

class FileSinkSpec extends Specification with BeforeAfterExample with FutureMatchers with Data {
  override def is = args(sequential = true) ^ super.is

  "FileSink" should {
    "write data" in {
      val (_, result) = FileSink.write(dataFile, chunk)
      result must whenDelivered {
        (_: Unit) => {
          (dataFile.exists must_==(true)) and 
          (dataFile.length mustEqual(data.flatten.length))
        }
      }
    }

    "cancel result when write failed" in {
      val error  = new RuntimeException
      val (_, result) = FileSink.write(dataFile, Right(ByteBuffer.wrap(data.head.toArray) :: Future[ByteBuffer](throw error).liftM[StreamT]))

      result.failed must whenDelivered {
        (err: Throwable) => (err must_== error) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }

    "cancel result when write getting next chunk failed" in {
      val error  = new RuntimeException
      val (_, result) = FileSink.write(dataFile, Right(ByteBuffer.wrap(data.head.toArray) :: (Promise.failed(error): Future[ByteBuffer]).liftM[StreamT]))

      result.failed must whenDelivered {
        (err: Throwable) => (err must_== error) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }

    "cancel writing when kill switch is thrown" in {
      val promise = Promise[ByteBuffer]()
      val (Some(killSwitch), result) = FileSink.write(dataFile, Right(ByteBuffer.wrap(data.head.toArray) :: (promise: Future[ByteBuffer]).liftM[StreamT]))

      val killed = new RuntimeException("killed")
      defaultActorSystem.scheduler.scheduleOnce(1000 millis) {
        killSwitch.kill(killed)
      }
      
      defaultActorSystem.scheduler.scheduleOnce(2000 millis) {
        promise.success(ByteBuffer.wrap(data.head.toArray))
      }

      Await.result(result.failed, 10 seconds) must beLike {
        case err: Throwable => (err must_== killed) and (dataFile.exists must_== true) and (dataFile.length must_== data.head.toArray.length)
      }
    }
  }

  protected def before = dataFile.delete

  protected def after = dataFile.delete
}

class FileSourceSpec extends Specification with Data with AfterExample with AkkaDefaults with FutureMatchers {
  "FileSource" should {
    "read all data" in{
      val (_, writeResult) = FileSink.write(dataFile, chunk)
      writeResult must whenDelivered (be_==(()))

      val readResult = new FileSource(dataFile).read
      ByteChunk.forceByteArray(readResult) must whenDelivered {
        be_==(data.flatten.toArray)
      }
    }
  }

  protected def after = dataFile.delete
}
