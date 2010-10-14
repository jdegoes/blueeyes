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
package blueeyes.json.xschema {

import _root_.org.specs.Specification
import _root_.org.specs.runner.{Runner, JUnit}

class DefaultSerializationExamplesTest extends Runner(DefaultSerializationExamples) with JUnit
object DefaultSerializationExamples extends Specification {
  import _root_.blueeyes.json.JsonAST._
  
  import DefaultSerialization._
  
  "Primitives can be extracted from strings" in {
    IntExtractor(JString("12")) mustEqual 12
    LongExtractor(JString("12")) mustEqual 12    
    FloatExtractor(JString("12.5")) mustEqual 12.5F
    DoubleExtractor(JString("12.5")) mustEqual 12.5
    BooleanExtractor(JString("true")) mustEqual true
    BooleanExtractor(JString("false")) mustEqual false
    BooleanExtractor(JString("0")) mustEqual false
    BooleanExtractor(JString("1")) mustEqual true
  }
  
  "Reals can be extracted from integers" in {
    FloatExtractor(JInt(12)) mustEqual 12.0F
    DoubleExtractor(JInt(12)) mustEqual 12.0
  }
  
  "Booleans can be extracted from integers" in {
    BooleanExtractor(JInt(0)) mustEqual false
    BooleanExtractor(JInt(1)) mustEqual true
  }
  
  "Integers can be extracted from reals" in {
    IntExtractor(JDouble(12.0)) mustEqual 12
    LongExtractor(JDouble(12.0)) mustEqual 12L
  }
  
  "Map of String to something is decomposed to object" in {
    val map = Map("foo" -> "bar")
    
    StringMapDecomposer(StringDecomposer).decompose(map) mustEqual JObject(JField("foo", JString("bar")) :: Nil)
  }
  
  "Map of String to something can be extracted from object" in {
    StringMapExtractor(StringExtractor).extract(JObject(JField("foo", JString("bar")) :: Nil)) mustEqual Map("foo" -> "bar")
  }
  
  "Order of elements in List is not changed" in {
    val l = List(1, 2, 3, 4, 5)
    
    val s = ListDecomposer(IntDecomposer).decompose(l)
    
    ListExtractor(IntExtractor).extract(s) mustEqual l
  }
  
  "Order of elements in Array is not changed" in {
    val l = Array(1, 2, 3, 4, 5)
    
    val s = ArrayDecomposer(IntDecomposer).decompose(l)
    
    ArrayExtractor(scala.reflect.ClassManifest.Int, IntExtractor).extract(s).toList mustEqual l.toList
  }
}


}

