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

import _root_.org.scalacheck.Prop._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Gen
import java.net.URLDecoder
import scala.util.control.Exception._
import java.nio.ByteBuffer
import scala.io.Source
import scalaz._
import scala.math.min
import scala.util.Random.nextInt
import scala.collection.mutable

object JsonParserSpec extends Specification with ArbitraryJValue with ScalaCheck {
  import JParser._

  "Any valid json can be parsed" in {
    val parsing = (json: JValue) => { parseUnsafe(json.renderPretty); true }
    check(parsing)
  }

  "Parsing is thread safe" in {
    import java.util.concurrent._

    val json = Examples.person
    val executor = Executors.newFixedThreadPool(100)
    val results = (0 to 100).map(_ =>
      executor.submit(new Callable[JValue] { def call = parseUnsafe(json) } )).toList.map(_.get)
    results.zip(results.tail).forall(pair => pair._1 == pair._2) mustEqual true
  }

  "All valid string escape characters can be parsed" in {
    parseUnsafe("[\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\\uffff\"]") must_== JArray(JString("abc\"\\/\b\f\n\r\t\u00a0\uffff")::Nil)
  }
}

object ParserBugs extends Specification {
  "Unicode ffff is a valid char in string literal" in {
    JParser.parseFromString(""" {"x":"\uffff"} """) must not(throwAn[java.lang.Exception])
  }

  "Does not hang when parsing 2.2250738585072012e-308" in {
    allCatch.opt(JParser.parseFromString(""" [ 2.2250738585072012e-308 ] """)) mustNotEqual None
  }

  "Does not hang when parsing 22.250738585072012e-309" in {
    allCatch.opt(JParser.parseFromString(""" [ 22.250738585072012e-309 ] """)) mustNotEqual None
  }

  "Can parse funky characters" in {
    JParser.parseUnsafe(URLDecoder.decode("\"%E2%84%A2\"", "UTF-8")) must_== JString("™")
  }
}

object ParsingByteBufferSpec extends Specification {
  "Respects current ByteBuffer's position" in {
    val bb = ByteBuffer.wrap(Array(54, 55, 56, 57))
    bb.remaining must_== 4
    bb.get must_== 54
    bb.remaining must_== 3
    JParser.parseFromByteBuffer(bb) must_== Success(JNum(789))
    bb.remaining must_== 0
  }

  "Respects current ByteBuffer's limit" in {
    val bb = ByteBuffer.wrap(Array(54, 55, 56, 57))
    bb.limit(3)
    JParser.parseFromByteBuffer(bb) must_== Success(JNum(678))
    bb.remaining must_== 0
    bb.limit(4)
    bb.remaining must_== 1
    JParser.parseFromByteBuffer(bb) must_== Success(JNum(9))
    bb.remaining must_== 0
  }
}

object AsyncParserSpec extends Specification {
  import JParser._

  val utf8 = java.nio.charset.Charset.forName("UTF-8")

  private def loadBytes(path: String): Array[Byte] =
    Source.fromFile(path).mkString.getBytes(utf8)

  private def chunk(data: Array[Byte], i: Int, j: Int) = {
    val len = min(j, data.length) - i
    if (len > 0) Some(ByteBuffer.wrap(data, i, len)) else None
  }

  private def chunkAll(async: AsyncParser, data: Array[Byte], f: () => Int) = {
    var vs = mutable.ArrayBuffer.empty[JValue]
    val n = data.length
    var i = 0
    var p = async
    while (i < n) {
      val step = f()
      val tpl = p(chunk(data, i, i + step))
      val (AsyncParse(errors, results), parser) = tpl
      if (!errors.isEmpty) sys.error("failed %s" format errors)
      vs ++= results
      p = parser
      i += step
    }
    vs
  }
  
  private def runTest(path: String, step: Int) = {
    val data = loadBytes(path)
    chunkAll(AsyncParser(), data, () => step)
  }
  
  private def runTestRandomStep(path: String, f: () => Int) = {
    val data = loadBytes(path)
    chunkAll(AsyncParser(), data, f)
  }
  
  "Async parser works on one 1M chunk" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 1024 * 1024)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 100K" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 100 * 1024)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 10K" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 10 * 1024)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 1K" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 1024)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 100B" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 100)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 10B" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 10)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of 1B" in {
    val vs = runTest("json/src/test/resources/z1k_nl.json", 1)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of sizes 10B-1K" in {
    val f = () => nextInt(1014) + 10
    val vs = runTestRandomStep("json/src/test/resources/z1k_nl.json", f)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  "Async parser works on chunks of sizes 1k-10K" in {
    val f = () => nextInt(9 * 1024) + 1024
    val vs = runTestRandomStep("json/src/test/resources/z1k_nl.json", f)
    vs.length must_== 1000
    (0 until 1000).foreach { _.toOption must beSome }
  }
  
  def run1(chunks: Seq[Option[ByteBuffer]], expected: Int) = {
    var p: AsyncParser = AsyncParser()
    var t0 = System.nanoTime
    var count = 0
    chunks.foreach { chunk =>
      val tpl = p(chunk)
      val (AsyncParse(errors, results), parser) = tpl
      if (!errors.isEmpty) sys.error("errors: %s" format errors)
      count += results.length
      p = parser
    }
    val t = System.nanoTime - t0
    if(count != expected) sys.error("wrong number of records")
    t
  }
  
  def run2(bb: ByteBuffer, expected: Int) = {
    val tt0 = System.nanoTime
    val v = JParser.parseManyFromByteBuffer(bb)
    val tt = System.nanoTime - tt0
    val seq = v.toOption.getOrElse(sys.error("failed to parse"))
    if(seq.length != expected) sys.error("wrong number of records")
    tt
  }
  
  "Async parser performs adequately" in {
    val n = 1 * 1000
    val data = loadBytes("json/src/test/resources/z1k_nl.json")

    val step = 100000
    println("parsing %d bytes with %d-byte chunks" format (data.length, step))
  
    def chunks = (0 until data.length by step).map(i => chunk(data, i, i + step))
    def bb = ByteBuffer.wrap(data)
  
    // warmup
    run1(chunks, n); run2(bb, n)
    run1(chunks, n); run2(bb, n)
    System.gc()
  
    val t1 = (0 until 10).foldLeft(0.0) { (t, _) => 
      val tt = run1(chunks, n); System.gc(); t + tt
    }
    println("async: %.2f ms" format (t1 / 10000000.0))
  
    val t2 = (0 until 10).foldLeft(0.0) { (t, _) => 
      val tt = run2(bb, n); System.gc(); t + tt
    }
    println("byteb: %.2f ms" format (t2 / 10000000.0))
  }

  "Async parser recovers from errors" in {
    val json = """{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999x}
{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999
{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999}
{"foo": 123, "bar": {"foo": 123, "bar": 999}}
{"foo": 123, "bar: {"foo": 123, "bar": 999}}
{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999}
xyz
{"foo": 123, "bar": 999}
{"foo": 123, "bar": 999}"""

    val bs = json.getBytes(utf8)
    val c = chunk(bs, 0, bs.length)

    var p = AsyncParser()
    val (AsyncParse(es, js), p2) = p(c)

    // each line should become an error or a jvalue
    json.split('\n').length must_== 14
    es.length must_== 4
    js.length must_== 10

    def confirm(e: ParseException, y: Int, x: Int) {
      e.line must_== y
      e.col must_== x
    }
    confirm(es(0), 2, 24)
    confirm(es(1), 6, 1)
    confirm(es(2), 9, 22)
    confirm(es(3), 12, 1)
  }
}
