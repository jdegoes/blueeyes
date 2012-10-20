/*
 * Copyright 2009-2010 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package blueeyes.json

import scalaz.{Failure,Success,Validation}
import scalaz.syntax.arrow._
import scalaz.std.function._

import scala.annotation.{switch, tailrec}
import scala.collection.mutable

import java.lang.Character.isHighSurrogate
import java.lang.Double.parseDouble
import java.lang.Integer.parseInt
import java.lang.Long.parseLong

import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.channels.ReadableByteChannel
import java.nio.charset.Charset

case class ParseException(msg: String, index: Int) extends Exception(msg)

trait JParser {
  import java.io._

  // legacy parsing methods
  @deprecated("Use parseFromString() instead, which returns a Validation", "1.0")
  def parse(str: String): JValue = new StringParser(str).parse()

  type R[A] = Validation[Throwable, A]

  // TODO: parsing from InputStream, ByteBuffer, etc
  // TODO: async parsing
  
  final def parseFromString(str: String): R[JValue] =
    Validation.fromTryCatch(new StringParser(str).parse())

  final def parseManyFromString(str: String): R[Seq[JValue]] =
    Validation.fromTryCatch(new StringParser(str).parseMany())

  final def parseFromFile(file: File): R[JValue] = Validation.fromTryCatch {
    new ChannelParser(new FileInputStream(file).getChannel).parse()
  }

  final def parseManyFromFile(file: File): R[Seq[JValue]] = Validation.fromTryCatch {
    new ChannelParser(new FileInputStream(file).getChannel).parseMany()
  }

  final def parseFromByteBuffer(buf: ByteBuffer): R[JValue] =
    Validation.fromTryCatch(new ByteBufferParser(buf).parse())

  final def parseManyFromByteBuffer(buf: ByteBuffer): R[Seq[JValue]] =
    Validation.fromTryCatch(new ByteBufferParser(buf).parseMany())
}
object JParser extends JParser

// underlying parser code adapted from jawn under MIT license.
// (https://github.com/non/jawn)

/**
 * Parser contains the state machine that does all the work. The only 
 */
private[json] trait Parser {

  final val utf8 = Charset.forName("UTF-8")

  /**
   * Read the byte/char at 'i' as a Char.
   *
   * Note that this should not be used on potential multi-byte sequences.
   */
  def at(i: Int): Char

  /**
   * Read the bytes/chars from 'i' until 'j' as a String.
   */
  def at(i: Int, j: Int): String

  /**
   * Return true iff 'i' is at or beyond the end of the input (EOF).
   */
  def atEof(i: Int): Boolean

  /**
   * Return true iff the byte/char at 'i' is equal to 'c'.
   */
  final def is(i: Int, c: Char): Boolean = at(i) == c

  /**
   * Return true iff the bytes/chars from 'i' until 'j' are equal to 'str'.
   */
  final def is(i: Int, j: Int, str: String): Boolean = at(i, j) == str

  /**
   * The reset() method is used to signal that we're working from the given
   * position, and any previous data can be released. Some parsers (e.g.
   * StringParser) will ignore release, while others (e.g. PathParser) will
   * need to use this information to release and allocate different areas.
   */
  def reset(i: Int): Int

  /**
   * Should be called when parsing is finished.
   */
  def close(): Unit

  /**
   * Valid parser states.
   */
  @inline final val ARRBEG = 6
  @inline final val OBJBEG = 7
  @inline final val DATA = 1
  @inline final val KEY = 2
  @inline final val SEP = 3
  @inline final val ARREND = 4
  @inline final val OBJEND = 5

  /**
   * Used to generate error messages with character info and byte addresses.
   */
  protected[this] def die(i: Int, msg: String) =
    throw ParseException("%s got %s (%d)" format (msg, at(i), i), i)

  /**
   * Used to generate messages for internal errors.
   */
  protected[this] def error(msg: String) =
    sys.error(msg)

  /**
   * Parse the given number, and add it to the given context.
   *
   * We don't actually instantiate a number here, but rather save the string
   * for future use. This ends up being way faster and has the nice side-effect
   * that we know exactly how the user represented the number.
   *
   * It would probably be possible to keep track of the whether the number is
   * expected to be whole, decimal, etc. but we don't do that at the moment.
   */
  final def parseNum(i: Int, ctxt: Context): Int = {
    var j = i
    var c = at(j)

    if (c == '-') {
      j += 1
      c = at(j)
    }
    while ('0' <= c && c <= '9') { j += 1; c = at(j) }

    if (c == '.') {
      j += 1
      c = at(j)
      while ('0' <= c && c <= '9') { j += 1; c = at(j) }
    }

    if (c == 'e' || c == 'E') {
      j += 1
      c = at(j)
      if (c == '+' || c == '-') {
        j += 1
        c = at(j)
      }
      while ('0' <= c && c <= '9') { j += 1; c = at(j) }
    }

    ctxt.add(JNum(at(i, j)))
    j
  }

  /**
   * This number parser is a bit slower because it has to be sure it doesn't
   * run off the end of the input. Normally (when operating in rparse in the
   * context of an outer array or objedct) we don't have to worry about this
   * and can just grab characters, because if we run out of characters that
   * would indicate bad input.
   *
   * This method has all the same caveats as the previous method.
   */
  final def parseNumSlow(i: Int, ctxt: Context): Int = {
    var j = i
    var c = at(j)

    if (c == '-') {
      // any valid input will require at least one digit after -
      j += 1
      c = at(j)
    }
    while ('0' <= c && c <= '9') {
      j += 1
      if (atEof(j)) {
        ctxt.add(JNum(at(i, j)))
        return j
      }
      c = at(j)
    }

    if (c == '.') {
      // any valid input will require at least one digit after .
      j += 1
      c = at(j)
      while ('0' <= c && c <= '9') {
        j += 1
        if (atEof(j)) {
          ctxt.add(JNum(at(i, j)))
          return j
        }
        c = at(j)
      }
    }

    if (c == 'e' || c == 'E') {
      // any valid input will require at least one digit after e, e+, etc
      j += 1
      c = at(j)
      if (c == '+' || c == '-') {
        j += 1
        c = at(j)
      }
      while ('0' <= c && c <= '9') {
        j += 1
        if (atEof(j)) {
          ctxt.add(JNum(at(i, j)))
          return j
        }
        c = at(j)
      }
    }
    ctxt.add(JNum(at(i, j)))
    j
  }

  /**
   * Generate a Char from the hex digits of "\u1234" (i.e. "1234").
   *
   * NOTE: This is only capable of generating characters from the basic plane.
   * This is why it can only return Char instead of Int.
   */
  final def descape(s: String) = parseInt(s, 16).toChar

  /**
   * Parse the JSON string starting at 'i' and save it into 'ctxt'.
   */
  def parseString(i: Int, ctxt: Context): Int

  /**
   * Parse the JSON constant "true".
   */
  final def parseTrue(i: Int) =
    if (is(i, i + 4, "true")) JTrue else die(i, "expected true")

  /**
   * Parse the JSON constant "false".
   */
  final def parseFalse(i: Int) =
    if (is(i, i + 5, "false")) JFalse else die(i, "expected false")

  /**
   * Parse the JSON constant "null".
   */
  final def parseNull(i: Int) =
    if (is(i, i + 4, "null")) JNull else die(i, "expected null")

  /**
   * Parse the JSON document into a single JSON value.
   *
   * The parser considers documents like '333', 'true', and '"foo"' to be
   * valid, as well as more traditional documents like [1,2,3,4,5]. However,
   * multiple top-level objects are not allowed.
   */
  final def parse(): JValue = {
    val (value, i) = parse(0)
    var j = i
    while (!atEof(j)) {
      (at(j): @switch) match {
        case ' ' => j += 1
        case '\t' => j += 1
        case '\n' => j += 1
        case _ => die(j, "expected whitespace")
      }
    }
    if (!atEof(j)) die(j, "expected eof")
    close()
    value
  }

  /**
   * Parse and return the "next" JSON value as well as the position beyond it.
   * This method is used by both parse() as well as parseMany().
   */
  final def parse(i: Int): (JValue, Int) = (at(i): @switch) match {
    // ignore whitespace
    case ' ' => parse(i + 1)
    case '\t' => parse(i + 1)
    case '\n' => parse(i + 1)

    // if we have a recursive top-level structure, we'll delegate the parsing
    // duties to our good friend rparse().
    case '[' => rparse(ARRBEG, i + 1, new ArrContext :: Nil)
    case '{' => rparse(OBJBEG, i + 1, new ObjContext :: Nil)

    // we have a single top-level number
    case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
      val ctxt = new SingleContext
      val j = parseNumSlow(i, ctxt)
      (ctxt.value, j)

    // we have a single top-level string
    case '"' =>
      val ctxt = new SingleContext
      val j = parseString(i, ctxt)
      (ctxt.value, j)

    // we have a single top-level constant
    case 't' => (parseTrue(i), i + 4)
    case 'f' => (parseFalse(i), i + 5)
    case 'n' => (parseNull(i), i + 4)

    // invalid
    case _ => die(i, "expected json value")
  }

  /**
   * Parse the given document into a sequence of JSON values. These might be
   * containers like objects and arrays, or primtitives like numbers and
   * strings.
   *
   * JSON objects may only be separated by whitespace. Thus, "top-level" commas
   * and other characters will become parse errors.
   */
  final def parseMany(): Seq[JValue] = {
    val results = mutable.ArrayBuffer.empty[JValue]
    var i = 0
    while (!atEof(i)) {
      (at(i): @switch) match {
        case ' ' | '\t' | '\n' => i += 1
        case _ =>
          val (value, j) = parse(i)
          results.append(value)
          i = j
      }
    }
    close()
    results
  }

  /**
   * Tail-recursive parsing method to do the bulk of JSON parsing.
   *
   * This single method manages parser states, data, etc. Except for parsing
   * non-recursive values (like strings, numbers, and constants) all important
   * work happens in this loop (or in methods it calls, like reset()).
   *
   * Currently the code is optimized to make use of switch statements. Future
   * work should consider whether this is better or worse than manually
   * constructed if/else statements or something else.
   */
  @tailrec
  final def rparse(state: Int, j: Int, stack: List[Context]): (JValue, Int) = {
    val i = reset(j)
    (state: @switch) match {
      // we are inside an object or array expecting to see data
      case DATA => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case '[' => rparse(ARRBEG, i + 1, new ArrContext :: stack)
        case '{' => rparse(OBJBEG, i + 1, new ObjContext :: stack)

        case '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          val ctxt = stack.head
          val j = parseNum(i, ctxt)
          rparse(if (ctxt.isObj) OBJEND else ARREND, j, stack)

        case '"' =>
          val ctxt = stack.head
          val j = parseString(i, ctxt)
          rparse(if (ctxt.isObj) OBJEND else ARREND, j, stack)

        case 't' =>
          val ctxt = stack.head
          ctxt.add(parseTrue(i))
          rparse(if (ctxt.isObj) OBJEND else ARREND, i + 4, stack)

        case 'f' =>
          val ctxt = stack.head
          ctxt.add(parseFalse(i))
          rparse(if (ctxt.isObj) OBJEND else ARREND, i + 5, stack)

        case 'n' =>
          val ctxt = stack.head
          ctxt.add(parseNull(i))
          rparse(if (ctxt.isObj) OBJEND else ARREND, i + 4, stack)

        case _ =>
          die(i, "expected json value")
      }

      // we are in an object expecting to see a key
      case KEY => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case '"' =>
          val j = parseString(i, stack.head)
          rparse(SEP, j, stack)

        case _ => die(i, "expected \"")
      }

      // we are starting an array, expecting to see data or a closing bracket
      case ARRBEG => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case ']' => stack match {
          case ctxt1 :: Nil =>
            (ctxt1.finish, i + 1)
          case ctxt1 :: ctxt2 :: tail =>
            ctxt2.add(ctxt1.finish)
            rparse(if (ctxt2.isObj) OBJEND else ARREND, i + 1, ctxt2 :: tail)
          case _ =>
            error("invalid stack")
        }

        case _ => rparse(DATA, i, stack)
      }

      // we are starting an object, expecting to see a key or a closing brace
      case OBJBEG => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case '}' => stack match {
          case ctxt1 :: Nil =>
            (ctxt1.finish, i + 1)
          case ctxt1 :: ctxt2 :: tail =>
            ctxt2.add(ctxt1.finish)
            rparse(if (ctxt2.isObj) OBJEND else ARREND, i + 1, ctxt2 :: tail)
          case _ =>
            error("invalid stack")
        }

        case _ => rparse(KEY, i, stack)
      }

      // we are in an object just after a key, expecting to see a colon
      case SEP => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case ':' => rparse(DATA, i + 1, stack)

        case _ => die(i, "expected :")
      }

      // we are at a possible stopping point for an array, expecting to see
      // either a comma (before more data) or a closing bracket.
      case ARREND => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case ',' => rparse(DATA, i + 1, stack)

        case ']' => stack match {
          case ctxt1 :: Nil =>
            (ctxt1.finish, i + 1)
          case ctxt1 :: ctxt2 :: tail =>
            ctxt2.add(ctxt1.finish)
            rparse(if (ctxt2.isObj) OBJEND else ARREND, i + 1, ctxt2 :: tail)
          case _ =>
            error("invalid stack")
        }

        case _ => die(i, "expected ] or ,")
      }

      // we are at a possible stopping point for an object, expecting to see
      // either a comma (before more data) or a closing brace.
      case OBJEND => (at(i): @switch) match {
        case ' ' => rparse(state, i + 1, stack)
        case '\t' => rparse(state, i + 1, stack)
        case '\n' => rparse(state, i + 1, stack)

        case ',' => rparse(KEY, i + 1, stack)

        case '}' => stack match {
          case ctxt1 :: Nil =>
            (ctxt1.finish, i + 1)
          case ctxt1 :: ctxt2 :: tail =>
            ctxt2.add(ctxt1.finish)
            rparse(if (ctxt2.isObj) OBJEND else ARREND, i + 1, ctxt2 :: tail)
          case _ =>
            error("invalid stack")
        }
        
        case _ => die(i, "expected } or ,")
      }
    }
  }
}


/**
 * Trait used when the data to be parsed is in UTF-16.
 */
trait CharBasedParser extends Parser {

  /**
   * See if the string has any escape sequences. If not, return the end of the
   * string. If so, bail out and return -1.
   *
   * This method expects the data to be in UTF-16 and accesses it as chars.
   * In a few cases we might bail out incorrectly (by reading the second-half
   * of a surrogate pair as \\) but for now the belief is that checking every
   * character would be more expensive. So... in those cases we'll fall back to
   * the slower (correct) UTF-16 parsing.
   */
  final def parseStringSimple(i: Int, ctxt: Context): Int = {
    var j = i
    var c = at(j)
    while (c != '"') {
      if (c == '\\') return -1
      j += 1
      c = at(j)
    }
    j + 1
  }

  /**
   * Parse the string according to JSON rules, and add to the given context.
   *
   * This method expects the data to be in UTF-16, and access it as Char. It
   * performs the correct checks to make sure that we don't interpret a
   * multi-char code point incorrectly.
   */
  final def parseString(i: Int, ctxt: Context): Int = {
    val k = parseStringSimple(i + 1, ctxt)
    if (k != -1) {
      ctxt.add(at(i + 1, k - 1))
      return k
    }

    var j = i + 1
    val sb = new CharBuilder
      
    var c = at(j)
    while (c != '"') {
      if (c == '\\') {
        (at(j + 1): @switch) match {
          case 'b' => { sb.append('\b'); j += 2 }
          case 'f' => { sb.append('\f'); j += 2 }
          case 'n' => { sb.append('\n'); j += 2 }
          case 'r' => { sb.append('\r'); j += 2 }
          case 't' => { sb.append('\t'); j += 2 }

          // if there's a problem then descape will explode
          case 'u' => { sb.append(descape(at(j + 2, j + 6))); j += 6 }

          // permissive: let any escaped char through, not just ", / and \
          case c2 => { sb.append(c2); j += 2 }
        }
      } else if (isHighSurrogate(c)) {
        // this case dodges the situation where we might incorrectly parse the
        // second Char of a unicode code point.
        sb.append(c)
        sb.append(at(j + 1))
        j += 2
      } else {
        // this case is for "normal" code points that are just one Char.
        sb.append(c)
        j += 1
      }
      j = reset(j)
      c = at(j)
    }
    ctxt.add(sb.makeString)
    j + 1
  }
}


/**
 * Trait used when the data to be parsed is in UTF-8.
 */
trait ByteBasedParser extends Parser {
  def byte(i: Int): Byte

  /**
   * See if the string has any escape sequences. If not, return the end of the
   * string. If so, bail out and return -1.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes. Thus
   * we can just ignore any bytes with the highest bit set.
   */
  final def parseStringSimple(i: Int, ctxt: Context): Int = {
    var j = i
    var c = byte(j)
    while (c != 34) {
      if (c == 92) return -1
      j += 1
      c = byte(j)
    }
    j + 1
  }

  /**
   * Parse the string according to JSON rules, and add to the given context.
   *
   * This method expects the data to be in UTF-8 and accesses it as bytes.
   */
  final def parseString(i: Int, ctxt: Context): Int = {
    val k = parseStringSimple(i + 1, ctxt)
    if (k != -1) {
      ctxt.add(at(i + 1, k - 1))
      return k
    }

    var j = i + 1
    val sb = new CharBuilder
      
    var c = byte(j)
    while (c != 34) { // "
      if (c == 92) { // \
        (byte(j + 1): @switch) match {
          case 98 => { sb.append('\b'); j += 2 }
          case 102 => { sb.append('\f'); j += 2 }
          case 110 => { sb.append('\n'); j += 2 }
          case 114 => { sb.append('\r'); j += 2 }
          case 116 => { sb.append('\t'); j += 2 }

          // if there's a problem then descape will explode
          case 117 => { sb.append(descape(at(j + 2, j + 6))); j += 6 }

          // permissive: let any escaped char through, not just ", / and \
          case c2 => { sb.append(c2.toChar); j += 2 }
        }
      } else if (c < 128) {
        // 1-byte UTF-8 sequence
        sb.append(c.toChar)
        j += 1
      } else if ((c & 224) == 192) {
        // 2-byte UTF-8 sequence
        sb.extend(at(j, j + 2))
        j += 2
      } else if ((c & 240) == 224) {
        // 3-byte UTF-8 sequence
        sb.extend(at(j, j + 3))
        j += 3
      } else if ((c & 248) == 240) {
        // 4-byte UTF-8 sequence
        sb.extend(at(j, j + 4))
        j += 4
      } else {
        die(j, "invalid UTF-8 encoding")
      }
      j = reset(j)
      c = byte(j)
    }
    ctxt.add(sb.makeString)
    j + 1
  }
}


/**
 * Basic in-memory string parsing.
 *
 * This parser is limited to the maximum string size (~2G). Obviously for large
 * JSON documents it's better to avoid using this parser and go straight from
 * disk, to avoid having to load the whole thing into memory at once.
 */
private[json] final class StringParser(s: String) extends CharBasedParser {
  final def reset(i: Int): Int = i
  final def at(i: Int): Char = s.charAt(i)
  final def at(i: Int, j: Int): String = s.substring(i, j)
  final def atEof(i: Int) = i == s.length
  final def close() = ()
}


/**
 * Basic ByteBuffer parser.
 */
private[json] final class ByteBufferParser(src: ByteBuffer) extends ByteBasedParser {
  val limit = src.limit

  final def close() = ()

  final def reset(i: Int): Int = i
  final def byte(i: Int): Byte = src.get(i)
  final def at(i: Int): Char = src.get(i).toChar

  final def at(i: Int, k: Int): String = {
    val len = k - i
    val arr = new Array[Byte](len)
    src.position(i)
    src.get(arr, 0, len)
    new String(arr, utf8)
  }

  final def atEof(i: Int) = i >= limit
}


/**
 * Basic file parser.
 *
 * Given a file name this parser opens it, chunks the data 1M at a time, and
 * parses it. 
 */
private[json] final class ChannelParser(ch: ReadableByteChannel) extends ByteBasedParser {

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

  final def atEof(i: Int) = if (i < bufsize) i >= ncurr else i >= nnext
}


/**
 * This class is used internally by AsyncParser to signal that we've reached
 * the end of the particular input we were given.
 */
private[json] class AsyncException extends Exception


/**
 * AsyncParser is a bit different from the others. Rather than explicitly
 * calling parse methods (which have no effect) you use the feed method to send
 * data to the parser, and get back intermediate results of the parsing.
 *
 * The format is the same as that accepted by parseMany: valid JSON objects
 * separated by whitespace. Data should be encoded in UTF-8 and supplied via an
 * instance of Option[ByteBuffer]. Some indicates there is more data to process
 * while None indicates an EOF.
 *
 * Upon encountering a parse error in a particular value, the parser adds the
 * exception to the sequence of results, then scans forward looking for an
 * opening { or [ and attempts to resume parsing.
 *
 * Note that certain values (e.g. numbers) cannot fully parse when the are the
 * final bytes of a buffer, since they could potentially be continued on the
 * next input.
 */
final class AsyncParser extends ByteBasedParser {

  // start with 128k
  var data: Array[Byte] = new Array[Byte](131072)
  var len = 0
  var allocated = 131072

  // this is our current position in bcurr
  var index = 0
  var done = false

  // consume the next bit of data, and return as many values as possible.
  final def feed(b: Option[ByteBuffer]): Seq[Validation[Throwable, JValue]] = {
    // if we don't have enough free space available we'll need to grow our
    // data array.
    // TODO: currently we never shrink data, assuming users will call feed
    // with similarly-sized bufs. this might be a bad assumption.
    b match {
      case None =>
        done = true

      case Some(buf) =>
        done = false
        val free = allocated - len
        val need = len + buf.capacity
        if (need > allocated) {
          val newdata = new Array[Byte](need)
          System.arraycopy(data, 0, newdata, 0, len)
          data = newdata
          allocated = need
        }
        // absorb the byte buffer's contents into our byte array
        buf.position(0)
        buf.get(data, len, buf.capacity)
        len = need
    }

    // this accumulates results that we'll eventually return
    val results = mutable.ArrayBuffer.empty[Validation[Throwable, JValue]]

    // we rely on exceptions to tell us when we run out of data
    try {
      while (true) {
        (at(index): @switch) match {
          // just pass whitespace by looking for the next value
          case ' ' | '\t' | '\n' =>
            index += 1

          // ok, let's try parsing at this point
          case _ =>
            try {
              index = reset(index)
              val (value, j) = parse(index)
              results.append(Success(value))
              index = j
            } catch {
              // we'll catch a parsing exception, note the error, and try to
              // make an effort to find the next value.
              case e: ParseException =>
                results.append(Failure(e))
                index = e.index
                // TODO: if we had a delimiter to look for we could do better
                while (at(index) != '{' && at(index) != '[') index += 1
            }
        }
      }
    } catch {
      case e: Exception =>
        // we ran out of data. index is storing the last place we started
        // parsing (or the last place we tried to start parsing) so we can just
        // return what we have, and resume later.

        // reset() in case we're done with our current buffer
        index = reset(index)
    }
    results
  }

  // every 65k bytes we shift our array back by 65k.
  final def reset(i: Int): Int = {
    if (i >= 65536) {
      len -= 65536
      index -= 65536
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
  final def byte(i: Int): Byte = if (i >= len)
    throw new AsyncException
  else
    data(i)

  // we need to signal if we got out-of-bounds
  final def at(i: Int): Char = if (i >= len)
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
  final def at(i: Int, k: Int): String = {
    if (k > len) throw new AsyncException
    val size = k - i
    val arr = new Array[Byte](size)
    System.arraycopy(data, i, arr, 0, size)
    new String(arr, utf8)
  }

  // the basic idea is that we don't signal EOF until done is true, which means
  // the client explicitly send us an EOF.
  final def atEof(i: Int) = if (done) i >= len else false

  // we don't have to do anything special on close.
  final def close() = ()
}
