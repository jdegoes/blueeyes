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
}

}
}