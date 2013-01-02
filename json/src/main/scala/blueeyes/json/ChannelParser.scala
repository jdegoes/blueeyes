package blueeyes.json

import java.io.{File, FileInputStream}
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel

object ChannelParser {
  private[json] def fromFile(f: File) =
    new ChannelParser(new FileInputStream(f).getChannel)
}

/**
 * Basic file parser.
 *
 * Given a file name this parser opens it, chunks the data 1M at a time, and
 * parses it. 
 */
private[json] final class ChannelParser(ch: ReadableByteChannel)
extends SyncParser with ByteBasedParser {

  // 256K buffers: arrived at via a bit of testing
  @inline final def bufsize = 262144
  @inline final def mask = bufsize - 1

  // these are the actual byte arrays we'll use
  var curr = new Array[Byte](bufsize)
  var next = new Array[Byte](bufsize)

  // these are the bytebuffers used to load the data
  var bcurr = ByteBuffer.wrap(curr)
  var bnext = ByteBuffer.wrap(next)

  // these are the bytecounts for each array
  var ncurr = ch.read(bcurr)
  var nnext = ch.read(bnext)

  var line = 0
  var pos = 0
  protected[this] final def newline(i: Int) { line += 1; pos = i }
  protected[this] final def column(i: Int) = i - pos

  final def close() = ch.close()

  /**
   * Swap the curr and next arrays/buffers/counts.
   *
   * We'll call this in response to certain reset() calls. Specifically, when
   * the index provided to reset is no longer in the 'curr' buffer, we want to
   * clear that data and swap the buffers.
   */
  final def swap() {
    var tmp = curr; curr = next; next = tmp
    var btmp = bcurr; bcurr = bnext; bnext = btmp
    var ntmp = ncurr; ncurr = nnext; nnext = ntmp
  }

  /**
   * If the cursor 'i' is past the 'curr' buffer, we want to clear the current
   * byte buffer, do a swap, load some more data, and continue.
   */
  final def reset(i: Int): Int = {
    if (i >= bufsize) {
      bcurr.clear()
      swap()
      nnext = ch.read(bnext)
      pos -= bufsize
      i - bufsize
    } else {
      i
    }
  }

  /**
   * This is a specialized accessor for the case where our underlying data are
   * bytes not chars.
   */
  final def byte(i: Int): Byte = if (i < bufsize)
    curr(i)
  else
    next(i & mask)

  /**
   * Rads
   */
  final def at(i: Int): Char = if (i < bufsize)
    curr(i).toChar
  else
    next(i & mask).toChar

  /**
   * Access a byte range as a string.
   *
   * Since the underlying data are UTF-8 encoded, i and k must occur on unicode
   * boundaries. Also, the resulting String is not guaranteed to have length
   * (k - i).
   */
  final def at(i: Int, k: Int): String = {
    val len = k - i

    if (k <= bufsize) {
      new String(curr, i, len, utf8)
    } else {
      val arr = new Array[Byte](len)
      val mid = bufsize - i
      System.arraycopy(curr, i, arr, 0, mid)
      System.arraycopy(next, 0, arr, mid, k - bufsize)
      new String(arr, utf8)
    }
  }

  final def atEof(i: Int) = if (i < bufsize) i >= ncurr else (i - bufsize) >= nnext
}
