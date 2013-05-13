package blueeyes.core

import blueeyes.util.metrics.DataSize

import blueeyes.bkka._
import akka.dispatch.Future
import akka.dispatch.Promise
import akka.dispatch.ExecutionContext

import java.util.Arrays
import java.nio.ByteBuffer

import scalaz._

import scala.math.max
import scala.collection.mutable.ListBuffer

package object data {
  type Chunk[A] = Either[A, StreamT[Future, A]]
  type ByteChunk = Chunk[Array[Byte]]

  object Chunk {
    implicit def tc(implicit M: Monad[Future]): Applicative[Chunk] = new Applicative[Chunk] {
      def point[A](a: => A) = Left(a)
      override def map[A, B](c: Chunk[A])(f: A => B): Chunk[B] = c match {
        case Left(a) => Left(f(a))
        case Right(stream) => Right(stream.map(f))
      }

      def ap[A, B](c: => Chunk[A])(f: => Chunk[A => B]) = {
        c match {
          case Left(a) =>
            f match {
              case Left(f0) => Left(f0(a))
              case Right(fx) => Right(fx.map(_(a)))
            }
          case Right(stream) =>
            f match {
              case Left(f0) => Right(stream map f0)
              case Right(fx) => Right(for (a <- stream; f0 <- fx) yield f0(a))
            }
        }
      }
    }
  }

  object ByteChunk {
    val defaultChunkSize = 8192

    def apply(data: Array[Byte]): ByteChunk = Left(data)

    def apply(data: Seq[Array[Byte]])(implicit executor: ExecutionContext): ByteChunk = {
      implicit val M: Monad[Future] = new FutureMonad(executor)

      Right(data.reverse.foldLeft(StreamT.empty[Future, Array[Byte]]) {
        case (stream, arr) => arr :: stream
      })
    }

    def aggregate(s: ByteChunk, chunkSize: Int = 8192)(implicit executor: ExecutionContext): ByteChunk = {
      implicit val M: Monad[Future] = new FutureMonad(executor)

      import StreamT._

      def aggregateStream(stream: StreamT[Future, Array[Byte]], buf: Array[Byte], offset: Int): Future[StreamT[Future, Array[Byte]]] = {
        stream.uncons.flatMap {
          case None =>
            if (offset > 0)
              Promise.successful(Arrays.copyOf(buf, offset) :: StreamT.empty[Future, Array[Byte]])
            else
              Promise.successful(StreamT.empty[Future, Array[Byte]])
          case Some((bytes, tail)) =>
            val limit = buf.length - offset
            if (bytes.length <= limit) {
              System.arraycopy(bytes, 0, buf, offset, bytes.length)
              aggregateStream(tail, buf, offset + bytes.length)
            } else if (offset > 0) {
              val copy = Arrays.copyOf(buf, offset)
              aggregateStream(tail, new Array[Byte](chunkSize), 0).map {
                copy :: bytes :: _
              }
            } else {
              aggregateStream(tail, new Array[Byte](chunkSize), 0).map { bytes :: _ }
            }
        }
      }

      s match {
        case Left(bytes) =>
          Left(bytes)
        case Right(stream) =>
          Right(StreamT.wrapEffect(aggregateStream(stream, new Array[Byte](chunkSize), 0)))
      }
    }

    def force(s: ByteChunk)(implicit executor: ExecutionContext): Future[Array[Array[Byte]]] = {
      implicit val M: Monad[Future] = new FutureMonad(executor)

      def loop(stream: StreamT[Future, Array[Byte]], buf: ListBuffer[Array[Byte]])(implicit M: Monad[Future]): Future[Array[Array[Byte]]] =
        stream.uncons.flatMap {
          case None =>
            Promise.successful(buf.toArray)
          case Some((bytes, tail)) =>
            buf.append(bytes)
            loop(tail, buf)
        }
      s match {
        case Right(stream) => loop(stream, ListBuffer.empty[Array[Byte]])
        case Left(bytes) => Promise.successful(Array(bytes))
      }
    }

    def forceByteArray(s: ByteChunk)(implicit executor: ExecutionContext): Future[Array[Byte]] = {
      implicit val M: Monad[Future] = new FutureMonad(executor)

      ByteChunk.force(s) map { arrays =>
        val len = arrays.foldLeft(0)(_ + _.length)
        val arr = new Array[Byte](len)
        var i = 0
        arrays.foreach { bytes =>
          System.arraycopy(bytes, 0, arr, i, bytes.length)
          i += bytes.length
        }
        arr
      }
    }

    def forceEagerChunk(s: ByteChunk)(implicit executor: ExecutionContext): Future[ByteChunk] =
      forceByteArray(s).map(Left(_))

    def chunkToStreamT(s: ByteChunk)(implicit executor: ExecutionContext): StreamT[Future, Array[Byte]] = {
      implicit val M: Monad[Future] = new FutureMonad(executor)
      s match {
        case Left(bytes) => bytes :: StreamT.empty[Future, Array[Byte]]
        case Right(stream) => stream
      }
    }

    private val empty: ByteChunk = Left(new Array[Byte](0))

    implicit def monoid(implicit executor: ExecutionContext): Monoid[ByteChunk] =
      new Monoid[ByteChunk] {
        implicit private val M: Monad[Future] = new FutureMonad(executor)

        val zero: ByteChunk = ByteChunk.empty

        def append(c0: ByteChunk, c1: => ByteChunk): ByteChunk =
          c0 match {
            case Left(bytes0) => c1 match {
              case Left(bytes1) =>
                val len0 = bytes0.length
                val len1 = bytes1.length
                val bytes = new Array[Byte](len0 + len1)
                System.arraycopy(bytes0, 0, bytes, 0, len0)
                System.arraycopy(bytes1, 0, bytes, len0, len1)
                Left(bytes)
              case Right(stream1) =>
                Right(bytes0 :: stream1)
            }

            case Right(stream0) =>
              Right(stream0 ++ chunkToStreamT(c1))
          }
      }
  }
}
