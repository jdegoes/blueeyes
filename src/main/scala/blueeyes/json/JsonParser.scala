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

/** Fast imperative parser.
 */
object JsonParser {
  import JsonAST._
  import java.io._

  class ParseException(message: String, cause: Exception) extends Exception(message, cause)

  /** Parsed tokens from low level pull parser.
   */
  sealed abstract class Token
  case object OpenObj extends Token
  case object CloseObj extends Token
  case class FieldStart(name: String) extends Token
  case object End extends Token
  case class StringVal(value: String) extends Token
  case class IntVal(value: BigInt) extends Token
  case class DoubleVal(value: Double) extends Token
  case class BoolVal(value: Boolean) extends Token
  case object NullVal extends Token
  case object OpenArr extends Token
  case object CloseArr extends Token

  /** Return parsed JSON.
   * @throws ParseException is thrown if parsing fails
   */
  def parse(s: String): JValue = parse(new Buffer(new StringReader(s)))

  /** Return parsed JSON.
   * @throws ParseException is thrown if parsing fails
   */
  def parse(s: Reader): JValue = parse(new Buffer(s))

  /** Return parsed JSON.
   */
  def parseOpt(s: String): Option[JValue] = try { Some(parse(s)) } catch { case e: Exception => None }

  /** Return parsed JSON.
   */
  def parseOpt(s: Reader): Option[JValue] = try { Some(parse(s)) } catch { case e: Exception => None }

  /** Parse in pull parsing style.
   * Use <code>p.nextToken</code> to parse tokens one by one from a string.
   * @see blueeyes.json.JsonParser.Token
   */
  def parse[A](s: String, p: Parser => A): A = parse(new StringReader(s), p)

  /** Parse in pull parsing style.
   * Use <code>p.nextToken</code> to parse tokens one by one from a stream.
   * @see blueeyes.json.JsonParser.Token
   */
  def parse[A](s: Reader, p: Parser => A): A = p(new Parser(new Buffer(s)))

  private def parse(buf: Buffer): JValue = {
    try {
      astParser(new Parser(buf))
    } catch {
      case e: ParseException => throw e
      case e: Exception => throw new ParseException("parsing failed", e)
    } finally { buf.release }
  }
  
  private[json] def unquote(string: String): String = unquote(new JsonParser.Buffer(new java.io.StringReader(string)))
  
  private[json] def unquote(buf: JsonParser.Buffer): String = {
    def unquote0(buf: JsonParser.Buffer, base: String): String = {
      val s = new java.lang.StringBuilder(base)
      var cOpt: Option[Char] = Some('\\')
      while (cOpt.isDefined) {
        val c = cOpt.get
        if (c == '"') {
          return s.toString
        }

        if (c == '\\') {
          buf.next match{
            case Some(c) =>
              c match {
                case '"'  => s.append('"')
                case '\\' => s.append('\\')
                case '/'  => s.append('/')
                case 'b'  => s.append('\b')
                case 'f'  => s.append('\f')
                case 'n'  => s.append('\n')
                case 'r'  => s.append('\r')
                case 't'  => s.append('\t')
                case 'u' =>
                  val chars = Array(buf.next, buf.next, buf.next, buf.next).filter(_.isDefined).map(_.get)
                  val codePoint = Integer.parseInt(new String(chars), 16)
                  s.appendCodePoint(codePoint)
                case _ => s.append('\\')
              }
            case None =>              
          }
        } else s.append(c)
        cOpt = buf.next
      }
      sys.error("expected string end")
    }
    
    buf.mark
    var cOpt = buf.next
    while (cOpt.isDefined) {
      val c = cOpt.get
      if (c == '"') return buf.substring
      if (c == '\\') {
        return unquote0(buf, buf.substring)
      }
      cOpt = buf.next
    }
    sys.error("expected string end")
  }

  // FIXME fail fast to prevent infinite loop, see
  // http://www.exploringbinary.com/java-hangs-when-converting-2-2250738585072012e-308/
  val BrokenDouble = BigDecimal("2.2250738585072012e-308")
  private[json] def parseDouble(s: String) = {
    val d = BigDecimal(s)
    if (d == BrokenDouble) sys.error("Error parsing 2.2250738585072012e-308")
    else d.doubleValue
  }

  private val astParser = (p: Parser) => {
    val vals = new ValStack(p)
    var token: Token = null
    var root: Option[JValue] = None

    // This is a slightly faster way to correct order of fields and arrays than using 'map'.
    def reverse(v: JValue): JValue = v match {
      case JObject(l) => JObject(l.map(f => JField(f.name, reverse(f.value))).reverse)
      case JArray(l)  => JArray(l.map(reverse).reverse)
      case x => x
    }

    def closeBlock(v: Either[JValue, JField]) {
      vals.popOption.map {
        case Right(topField @ JField(name, _)) => v match {
          case Left(jvalue)  => vals.push(JObject(JField(name, jvalue) :: vals.pop(JObject).fields))
          case Right(jfield) => p.fail("Consecutive JFields on parser stack: " + topField + ", " + jfield)
        }

        case Left(JObject(fields)) => v match {
          case Left(jvalue)  => p.fail("Expected JField but got " + jvalue)
          case Right(jfield) => vals.push(JObject(jfield :: fields))
        }

        case Left(JArray(elements)) => v match {
          case Left(jvalue)  => vals.push(JArray(jvalue :: elements))
          case Right(jfield) => p.fail("Cannot add " + jfield + " to JArray")
        }

        case Left(jvalue) => p.fail("Expected JArray, JObject, or JField but got " + jvalue)
      } getOrElse {
        root = v match {
          case Left(jvalue)  => Some(reverse(jvalue))
          case Right(jfield) => p.fail("Root of tree cannot be a JField.")
        }
      }
    }

    def mergeTop(jvalue: JValue) {
      vals.popOption map {
        case Right(JField(name, _)) => vals.push(JObject(JField(name, jvalue) :: vals.pop(JObject).fields))
        case Left(JArray(elements)) => vals.push(JArray(jvalue :: elements))
        case Left(jvalue) => p.fail("Expected JArray or JField but got " + jvalue)
      } getOrElse {
        root = Some(jvalue)
      }
    }

    do {
      token = p.nextToken
      token match {
        case OpenObj          => vals.push(JObject(Nil))
        case FieldStart(name) => vals.pushField(JField(name, null))
        case StringVal(x)     => mergeTop(JString(x))
        case IntVal(x)        => mergeTop(JInt(x))
        case DoubleVal(x)     => mergeTop(JDouble(x))
        case BoolVal(x)       => mergeTop(JBool(x))
        case NullVal          => mergeTop(JNull)
        case CloseObj         => closeBlock(vals.pop)
        case OpenArr          => vals.push(JArray(Nil))
        case CloseArr         => closeBlock(vals.pop)
        case End              =>
      }
    } while (token != End)

    root.getOrElse(throw new ParseException("Expected JObject or JArray", null))
  }

  private class ValStack(parser: Parser) {
    import java.util.LinkedList
    private[this] var stack = new LinkedList[Either[JValue, JField]]()

    def pop = stack.pop

    def popOption = if (stack.isEmpty) None else Some(stack.pop)

    def pop[A <: JValue](expectedType: JManifest{type JType = A}): A = stack.pop match {
      case Left(jvalue)  => expectedType(jvalue).getOrElse(parser.fail("Expected " + expectedType + " but got " + jvalue))
      case Right(jfield) => parser.fail("Expected " + expectedType + " but got " + jfield)
    }

    def push(v: JValue) = stack.push(Left(v))
    def pushField(v: JField) = stack.push(Right(v))
  }

  class Parser(buf: Buffer) {
    import java.util.LinkedList

    private[this] val blocks = new LinkedList[BlockMode]()
    private[this] var fieldNameMode = true

    def fail(msg: String) = throw new ParseException(msg + "\nNear: " + buf.near, null)

    /** Parse next Token from stream.
     */
    def nextToken: Token = {
      def isDelimiter(c: Char) = c == ' ' || c == '\n' || c == ',' || c == '\r' || c == '\t' || c == '}' || c == ']'

      def parseString: String = {
        try {
          unquote(buf)
        }
        catch {
          case _ => fail("unexpected string end")
        }
      }

      def parseValue(first: Char) = {
        var wasInt = true
        var doubleVal = false
        val s = new StringBuilder
        s.append(first)
        while (wasInt) {
          buf.next match{
            case Some(c) =>{
              if (c == '.' || c == 'e' || c == 'E') {
                doubleVal = true
                s.append(c)
              } else if (!(Character.isDigit(c) || c == '.' || c == 'e' || c == 'E' || c == '-')) {
                wasInt = false
                buf.back
              } else s.append(c)
            }
            case None => wasInt = false
          }
        }
        val value = s.toString
        if (doubleVal) DoubleVal(parseDouble(value))
        else IntVal(BigInt(value))
      }

      def readString(charCount: Int) = {
        val chars = for (i <- 1 to charCount) yield buf.next
        chars.filter(_.isDefined).map(_.get).mkString("")
      }

      while (true) {
        val cOpt = buf.next
        cOpt match{
          case None => return End
          case Some(c) =>
          c match {
            case '{' =>
              blocks.addFirst(OBJECT)
              fieldNameMode = true
              return OpenObj
            case '}' =>
              blocks.poll
              return CloseObj
            case '"' =>
              if (fieldNameMode && blocks.peek == OBJECT) return FieldStart(parseString)
              else {
                fieldNameMode = true
                return StringVal(parseString)
              }
            case 't' =>
              fieldNameMode = true
              if (readString(3) == "rue") {
                return BoolVal(true)
              }
              fail("expected boolean")
            case 'f' =>
              fieldNameMode = true
              if (readString(4) == "alse") {
                return BoolVal(false)
              }
              fail("expected boolean")
            case 'n' =>
              fieldNameMode = true
              if (readString(3) == "ull") {
                return NullVal
              }
              fail("expected null")
            case ':' =>
              fieldNameMode = false
            case '[' =>
              blocks.addFirst(ARRAY)
              return OpenArr
            case ']' =>
              fieldNameMode = true
              blocks.poll
              return CloseArr
            case c if Character.isDigit(c) || c == '-' =>
              fieldNameMode = true
              return parseValue(c)
            case c if isDelimiter(c) =>
            case c => fail("unknown token " + c)
          }
        }
      }
      End
    }

    sealed abstract class BlockMode
    case object ARRAY extends BlockMode
    case object OBJECT extends BlockMode
  }

  /* Buffer used to parse JSON.
   * Buffer is divided to one or more segments (preallocated in Segments pool).
   */
  private[json] class Buffer(in: Reader) {
    var length = -1
    var curMark = -1
    var curMarkSegment = -1
    private[this] var segments: List[Segment] = Nil
    private[this] var segment: Array[Char] = _
    private[this] var cur = 0 // Pointer which points current parsing location
    private[this] var curSegmentIdx = 0 // Pointer which points current segment
    read

    def mark = { curMark = cur; curMarkSegment = curSegmentIdx }
    def back = cur = cur-1

    def next: Option[Char] = {
      try {
        val c = segment(cur)
        if (cur >= length) return None
        cur = cur+1
        Some(c)
      } catch {
        // suprisingly catching IndexOutOfBounds is faster than: if (cur == segment.length)
        case e =>
          read
          if (length == -1) None else next
      }
    }

    def substring = {
      if (curSegmentIdx == curMarkSegment) new String(segment, curMark, cur-curMark-1)
      else { // slower path for case when string is in two or more segments
        var parts: List[(Int, Int, Array[Char])] = Nil
        var i = curSegmentIdx
        while (i >= curMarkSegment) {
          val s = segments(i).seg
          val start = if (i == curMarkSegment) curMark else 0
          val end = if (i == curSegmentIdx) cur else s.length+1
          parts = (start, end, s) :: parts
          i = i-1
        }
        val len = parts.map(p => p._2 - p._1 - 1).foldLeft(0)(_ + _)
        val chars = new Array[Char](len)
        i = 0
        var pos = 0

        while (i < parts.size) {
          val (start, end, b) = parts(i)
          val partLen = end-start-1
          System.arraycopy(b, start, chars, pos, partLen)
          pos = pos + partLen
          i = i+1
        }
        new String(chars)
      }
    }

    def near = new String(segment, (cur-20) max 0, (cur+20) min length)

    def release = segments.foreach(Segments.release)

    private[this] def read = {
      try {
        val newSegment = Segments.apply()
        length = in.read(newSegment.seg)
        segment = newSegment.seg
        segments = segments ::: List(newSegment)
        cur = 0
        curSegmentIdx = segments.length-1
      } finally {
        if (length < segment.length) in.close
      }
    }
  }

  /* A pool of preallocated char arrays.
   */
  private[json] object Segments {
    import java.util.concurrent.ArrayBlockingQueue
    import java.util.concurrent.atomic.AtomicInteger

    private[json] var segmentSize = 1000
    private[this] val maxNumOfSegments = 10000
    private[this] var segmentCount = new AtomicInteger(0)
    private[this] val segments = new ArrayBlockingQueue[Segment](maxNumOfSegments)
    private[json] def clear = segments.clear

    def apply(): Segment = {
      val s = acquire
      // Give back a disposable segment if pool is exhausted.
      if (s != null) s else DisposableSegment(new Array(segmentSize))
    }

    private[this] def acquire: Segment = {
      val curCount = segmentCount.get
      val createNew = 
        if (segments.size == 0 && curCount < maxNumOfSegments)
          segmentCount.compareAndSet(curCount, curCount + 1)
        else false

      if (createNew) RecycledSegment(new Array(segmentSize)) else segments.poll
    }
      def release(s: Segment) = s match {
        case _: RecycledSegment => segments.offer(s)
        case _ =>
      }
    }

  sealed trait Segment {
    val seg: Array[Char]
  }
  case class RecycledSegment(seg: Array[Char]) extends Segment
  case class DisposableSegment(seg: Array[Char]) extends Segment
}
