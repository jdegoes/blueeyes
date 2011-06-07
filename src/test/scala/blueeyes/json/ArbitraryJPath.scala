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

import org.scalacheck.{Gen, Arbitrary}
import Arbitrary.arbitrary

trait ArbitraryJPath {
  def trace[A](a: => A): A = try { a } catch {
    case ex => { ex.printStackTrace; throw ex }
  }

  val genJPath = {
    import Gen._

    val genIndex = for {
      index <- choose(0, 10)
    } yield JPathIndex(index)

    val genField = for {
      name <- identifier
    } yield JPathField(name)

    val genIndexOrField = Gen.oneOf(genIndex, genField)

    for {
      length      <- choose(1, 10)
      listOfNodes <- listOfN(length, genIndexOrField)
    } yield JPath(listOfNodes)
  }

  implicit val arbJPath: Arbitrary[JPath] = Arbitrary(genJPath)
}
object ArbitraryJPath extends ArbitraryJPath
