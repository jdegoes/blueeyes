/*
 * Copyright 2010 John A. De Goes
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
 
package blueeyes {
package json {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

import JsonAST._

import scala.util.matching.Regex

class JPathSpecTest extends Runner(JPathSpec) with JUnit

object JPathSpec extends Specification {
  "Parses nested field access" in {
    JPath(".foo.bar").nodes mustEqual (JPathField("foo") :: JPathField("bar") :: Nil)
  }
  
  "Parses nested field access with arrays" in {
    JPath(".foo.bar[2].baz[9][1]").nodes mustEqual (JPathField("foo") :: JPathField("bar") :: JPathIndex(2) :: JPathField("baz") :: JPathIndex(9) :: JPathIndex(1) :: Nil)
  }
  
  "Forgivingly parses initial field name without leading dot" in {
    JPath("foo.bar").nodes mustEqual (JPathField("foo") :: JPathField("bar") :: Nil)
  }
  
  "Accurately detects the wildcard operator" in {
    JPath("foo.*").nodes mustEqual (JPathField("foo") :: JPathRegex("(.*)".r) :: Nil)
  }
  
  "Detects a basic regular expression in between two fields" in {
    JPath("foo./bar/.baz").nodes mustEqual(JPathField("foo") :: JPathRegex("(bar)".r) :: JPathField("baz") :: Nil)
  }
  
  "Detects a basic leading regular expression" in {
    JPath("./ba[rz]/").nodes mustEqual(JPathRegex("(ba[rz])".r) :: Nil)
  }
  
  "Can extract a first-level regular expression" in {
    val j = JObject(JField("baz", JNull) :: JField("bar", JNull) :: JField("foo", JNull) :: Nil)
    
    JPath("./ba[rz]/").extract(j) mustEqual(JNull :: JNull :: Nil)
  }
  "Can extract a second level node" in {
    val j = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)

    JPath("address.city").extract(j) mustEqual(JString("B") :: Nil)
  }

  "Expand will accurately convert regular expressions for an object with depth = 1" in {
    val j = JObject(JField("baz", JNull) :: JField("bar", JNull) :: JField("foo", JNull) :: Nil)
    
    JPath("./ba[rz]/").expand(j) mustEqual (
      JPath(JPathField("baz")) :: 
      JPath(JPathField("bar")) :: Nil
    )
  }
}

}
}