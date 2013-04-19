package blueeyes.json

import scala.annotation.{switch, tailrec}
import scala.collection.mutable
import java.nio.ByteBuffer

case class AsyncParse(errors: Seq[ParseException], values: Seq[JValue])

/**
 * This class is used internally by AsyncParser to signal that we've reached
 * the end of the particular input we were given.
 */
private[json] class AsyncException extends Exception

private[json] class FailureException extends Exception

object AsyncParser {
  def apply(failFast: Boolean): AsyncParser =
    new AsyncParser(new Array[Byte](131072), 0, 131072, 0, false, failFast)
}

final class AsyncParser protected[json] (
  protected[json] var data: Array[Byte],
  protected[json] var len: Int,
  protected[json] var allocated: Int,
  protected[json] var index: Int,
  protected[json] var done: Boolean,
  protected[json] val failFast: Boolean
) extends ByteBasedParser {

  protected[this] var line = 0
  protected[this] var pos = 0
  protected[this] final def newline(i: Int) { line += 1; pos = i + 1 }
  protected[this] final def column(i: Int) = i - pos

  protected[this] final def copy() =
    new AsyncParser(data.clone, len, allocated, index, done, failFast)

  final def apply(b: Option[ByteBuffer]): (AsyncParse, AsyncParser) = copy.feed(b)

  protected[this] final def absorb(buf: ByteBuffer): Unit = {
    done = false
    val free = allocated - len
    val buflen = buf.limit - buf.position
    val need = len + buflen

    // if we don't have enough free space available we'll need to grow our
    // data array. we never shrink the data array, assuming users will call
    // feed with similarly-sized buffers.
    if (need > allocated) {
      val newdata = new Array[Byte](need)
      System.arraycopy(data, 0, newdata, 0, len)
      data = newdata
      allocated = need
    }

    buf.get(data, len, buflen)
    len = need
  }

  // if this runs off the end of at() we'll get an AsyncException, which is OK
  // as it will signal the end of the parse.
  protected[json] def recoverFromError(e: ParseException) {
    if (e.index > index && at(e.index - 1) == '\n') {
      index = e.index
    } else {
      while (at(index) != '\n') index += 1
    }
  }

  protected[json] def feed(b: Option[ByteBuffer]): (AsyncParse, AsyncParser) = {
    b match {
      case None => done = true
      case Some(buf) => absorb(buf)
    }

    // accumulates errors and results
    val errors = mutable.ArrayBuffer.empty[ParseException]
    val results = mutable.ArrayBuffer.empty[JValue]

    // we rely on exceptions to tell us when we run out of data
    try {
      while (true) {
        (at(index): @switch) match {
          // just pass whitespace by looking for the next value
          case '\n' =>
            newline(index)
            index += 1

          case ' ' | '\t' | '\r' =>
            index += 1

          // ok, let's try parsing at this point
          case _ =>
            try {
              index = reset(index)
              val (value, j) = parse(index)
              results.append(value)
              index = j
            } catch {
              // we'll catch a parsing exception, note the error, and try to
              // make an effort to find the next value.
              case e: ParseException =>
                errors.append(e)
                if (failFast) {
                  throw new AsyncException
                } else {
                  recoverFromError(e)
                }
            }
        }
      }
    } catch {
      case e: AsyncException =>
        // we ran out of data. index is storing the last place we started
        // parsing (or the last place we tried to start parsing) so we can just
        // return what we have, and resume later.

        // reset() in case we're done with our current buffer
        index = reset(index)

      case e: Exception =>
        // we got some other exception
        throw e
    }
    (AsyncParse(errors, results), this)
  }

  // every 65k bytes we shift our array back by 65k.
  protected[this] final def reset(i: Int): Int = {
    if (index >= 65536) {
      len -= 65536
      index -= 65536
      pos -= 65536
      System.arraycopy(data, 65536, data, 0, len)
      i - 65536
    } else {
      i
    }
  }

  /**
   * This is a specialized accessor for the case where our underlying data are
   * bytes not chars.
   */
  protected[this] final def byte(i: Int): Byte = if (i >= len)
    throw new AsyncException
  else
    data(i)

  // we need to signal if we got out-of-bounds
  protected[this] final def at(i: Int): Char = if (i >= len)
    throw new AsyncException
  else
    data(i).toChar

  /**
   * Access a byte range as a string.
   *
   * Since the underlying data are UTF-8 encoded, i and k must occur on unicode
   * boundaries. Also, the resulting String is not guaranteed to have length
   * (k - i).
   */
  protected[this] final def at(i: Int, k: Int): String = {
    if (k > len) throw new AsyncException
    val size = k - i
    val arr = new Array[Byte](size)
    System.arraycopy(data, i, arr, 0, size)
    new String(arr, utf8)
  }

  // the basic idea is that we don't signal EOF until done is true, which means
  // the client explicitly send us an EOF.
  protected[this] final def atEof(i: Int) = if (done) i >= len else false

  // we don't have to do anything special on close.
  protected[this] final def close() = ()
}
