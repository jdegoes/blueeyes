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
import scalaz.Monoid
import scalaz.Scalaz._
import scalaz.std.tuple._
import scalaz.std.list._

trait ArbitraryJPath {
  def trace[A](a: => A): A = try { a } catch {
    case ex => { ex.printStackTrace; throw ex }
  }

  import Gen._

  implicit val tupleJPathApp = Monoid[(String, List[JPathNode])].applicative

  def path: Gen[(String, List[JPathNode])] = {
    for {
      p  <- jIdentifier.map { id => (id, List(JPathField(id))) } | field
      ps <- resize(10, listOf(field).map { ps => tupleJPathApp.traverse(ps) { x => x } })
    } yield p |+| ps
  }

  def field: Gen[(String, List[JPathNode])] =
    for ((s, p) <- dotField | indexField) yield (s, List(p))


  def dotField: Gen[(String, JPathField)] =
    for {
      str <- jIdentifier
    } yield ("." + str -> JPathField(str))


  def indexField: Gen[(String, JPathIndex)] = {
    def dqString: Gen[(String, JPathIndex)] = {
      for {
        str <- Arbitrary.arbitrary[String]
        qs   = str.replace("""\""", """\\""").replace("\"", "\\\"")
      } yield ("[\"" + qs + "\"]" -> JPathIndex(qs))
    }

    def sqString: Gen[(String, JPathIndex)] = {
      for {
        str <- Arbitrary.arbitrary[String]
        qs   = str.replace("""\""", """\\""").replace("'", """\'""")
      } yield ("['" + qs + "']" -> JPathIndex(qs))
    }

    def number: Gen[(String, JPathIndex)] = {
      for {
        idx <- Gen.choose(0, 1000)
        qs   = idx.toString
      } yield ("[" + qs + "]" -> JPathIndex(idx))
    }

    dqString | sqString | number
  }


  def jIdentifier: Gen[String] = {
    def identifierStart: Gen[Char] =
      oneOf(
        choose('a', 'z'),
        choose('A', 'Z'),
        '_',
        '$'
      )

    def identifierBody: Gen[Char] =
      identifierStart | choose('0', '9')

    for {
      c  <- identifierStart
      cs <- resize(10, listOf(identifierBody))
    } yield (c::cs).mkString
  }

  val genJPath = {
    for {
      (path, nodes) <- path
    } yield (path, JPath(nodes))
  }

  implicit val arbJPath: Arbitrary[(String, JPath)] = Arbitrary(genJPath)
}
object ArbitraryJPath extends ArbitraryJPath
