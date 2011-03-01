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
 
package blueeyes.json

import org.specs.{Specification, ScalaCheck}
import org.specs.runner.{Runner, JUnit}
import org.scalacheck.{Gen, Arbitrary, Prop}
import Prop.forAll
import Arbitrary.arbitrary

import JsonAST._

import scala.util.matching.Regex

class JPathSpecTest extends Runner(JPathSpec) with JUnit

object JPathSpec extends Specification with ScalaCheck {
  implicit val argJPath: Arbitrary[JPath] = Arbitrary {
    import Gen._

    val genIndex = for {
      index <- choose(0, 10)
    } yield JPathIndex(index)

    val genField = for {
      name <- identifier
    } yield JPathField(name)

    val genIndexOrField = Gen.oneOf(genIndex, genField)

    for {
      length      <- choose(0, 10)
      listOfNodes <- listOfN(length, genIndexOrField)
    } yield JPath(listOfNodes)
  }

  "Parser" should {
    "parse all valid JPath strings" in {
      forAll { (jpath: JPath) =>
        JPath(jpath.toString) mustEqual jpath
      } must pass
    }
  
    "forgivingly parse initial field name without leading dot" in {
      JPath("foo.bar").nodes mustEqual (JPathField("foo") :: JPathField("bar") :: Nil)
    }
  }
  
  "Extractor" should {
    "extract a second level node" in {
      val j = JObject(JField("address", JObject( JField("city", JString("B")) :: JField("street", JString("2")) ::  Nil)) :: Nil)

      JPath("address.city").extract(j) mustEqual(JString("B") :: Nil)
    }
  }
  
  "Parent" should {
    "return parent" in {
      JPath(".foo.bar").parent mustEqual Some(JPath(".foo"))
    }
    
    "return None when there is no parent" in {
      JPath(".foo").parent mustEqual None
    }
  }
  
  "Ancestors" should {
    "return two ancestors" in {
      JPath(".foo.bar.baz").ancestors mustEqual List(JPath(".foo.bar"), JPath(".foo"))
    }
    
    "return empty list" in {
      JPath(".foo").ancestors mustEqual Nil
    }
  }
}