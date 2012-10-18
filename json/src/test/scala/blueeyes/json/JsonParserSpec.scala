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

object JsonParserSpec extends Specification with ArbitraryJValue with ScalaCheck {
  import JParser._

  "Any valid json can be parsed" in {
    val parsing = (json: JValue) => { parse(json.renderPretty); true }
    check(parsing)
  }

  "Parsing is thread safe" in {
    import java.util.concurrent._

    val json = Examples.person
    val executor = Executors.newFixedThreadPool(100)
    val results = (0 to 100).map(_ =>
      executor.submit(new Callable[JValue] { def call = parse(json) } )).toList.map(_.get)
    results.zip(results.tail).forall(pair => pair._1 == pair._2) mustEqual true
  }

  "All valid string escape characters can be parsed" in {
    parse("[\"abc\\\"\\\\\\/\\b\\f\\n\\r\\t\\u00a0\\uffff\"]") must_== JArray(JString("abc\"\\/\b\f\n\r\t\u00a0\uffff")::Nil)
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
    JParser.parse(URLDecoder.decode("\"%E2%84%A2\"", "UTF-8")) must_== JString("™")
  }
}
