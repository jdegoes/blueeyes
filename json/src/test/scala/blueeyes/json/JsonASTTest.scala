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

import org.scalacheck._
import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import scalaz.Ordering._

object JsonASTSpec extends Specification with ScalaCheck with ArbitraryJPath with ArbitraryJValue {
  override val defaultPrettyParams = Pretty.Params(2)

  "Functor identity" in {
    val identityProp = (json: JValue) => json == (json mapUp identity)
    check(identityProp)
  }

  "Functor composition" in {
    val compositionProp = (json: JValue, fa: JValue => JValue, fb: JValue => JValue) =>
      json.mapUp(fb).mapUp(fa) == json.mapUp(fa compose fb)

    check(compositionProp)
  }

  "Monoid identity" in {
    val identityProp = (json: JValue) => (json ++ JUndefined == json) && (JUndefined ++ json == json)
    check(identityProp)
  }

  "Monoid associativity" in {
    val assocProp = (x: JValue, y: JValue, z: JValue) => x ++ (y ++ z) == (x ++ y) ++ z
    check(assocProp)
  }

  "Merge identity" in {
    val identityProp = (json: JValue) => (json merge JUndefined) == json && (JUndefined merge json) == json
    check(identityProp)
  }

  "Merge idempotency" in {
    val idempotencyProp = (x: JValue) => (x merge x) == x
    check(idempotencyProp)
  }

  "Diff identity" in {
    val identityProp = (json: JValue) =>
      (json diff JUndefined) == Diff(JUndefined, JUndefined, json) &&
      (JUndefined diff json) == Diff(JUndefined, json, JUndefined)

    check(identityProp)
  }

  "Diff with self is empty" in {
    val emptyProp = (x: JValue) => (x diff x) == Diff(JUndefined, JUndefined, JUndefined)
    check(emptyProp)
  }

  "Diff is subset of originals" in {
    val subsetProp = (x: JObject, y: JObject) => {
      val Diff(c, a, d) = x diff y
      y == (y merge (c merge a))
    }
    check(subsetProp)
  }

  "Diff result is same when fields are reordered" in {
    val reorderProp = (x: JObject) => (x diff reorderFields(x)) == Diff(JUndefined, JUndefined, JUndefined)
    check(reorderProp)
  }

  "delete" in {
    JParser.parseUnsafe("""{ "foo": { "bar": 1, "baz": 2 } }""").delete(JPath("foo.bar")) must beSome(JParser.parseUnsafe("""{ "foo": { "baz": 2 } }"""))
  }

  "Remove all" in {
    val removeAllProp = (x: JValue) => (x remove { _ => true }) == JUndefined
    check(removeAllProp)
  }

  "Remove nothing" in {
    val removeNothingProp = (x: JValue) => (x remove { _ => false }) == x
    check(removeNothingProp)
  }

  "Remove removes only matching elements (in case of a field, the field is removed)" in {
    val removeProp = (json: JValue, x: Class[_ <: JValue]) => {
      val removed = json remove typePredicate(x)
      val Diff(c, a, d) = json diff removed

      removed.flatten.forall(_.getClass != x)
    }
    check(removeProp)
  }

  "flattenWithPath includes empty object values" in {
    val test = JObject(JField("a", JObject(Nil)) :: Nil)

    val expected = List((JPath(".a"), JObject(Nil)))

    test.flattenWithPath must_== expected
  }
  
  "flattenWithPath includes empty array values" in {
    val test = JObject(JField("a", JArray(Nil)) :: Nil)

    val expected = List((JPath(".a"), JArray(Nil)))

    test.flattenWithPath must_== expected
  }

  "flattenWithPath for values produces a single value with the identity path" in {
    val test = JNum(1)

    val expected = List((JPath.Identity, test))

    test.flattenWithPath must_== expected
  }

  "flattenWithPath on arrays produces index values" in {
    val test = JArray(JNum(1) :: Nil)

    val expected = List((JPath("[0]"), JNum(1)))

    test.flattenWithPath must_== expected
  }

  "flattenWithPath does not produce JUndefined entries" in {
    val test = JParser.parseUnsafe("""{
      "c":2,
      "fn":[{
        "fr":-2
      }]
    }""")

    val expected = List(
      JPath(".c") -> JNum("2"),
      JPath(".fn[0].fr") -> JNum("-2")
    )

    test.flattenWithPath must_== expected
  }

  "unflatten is the inverse of flattenWithPath" in {
    val inverse = (value: JValue) => JValue.unflatten( value.flattenWithPath ) == value 

    check(inverse)
  }

  "Set and retrieve an arbitrary jvalue at an arbitrary path" in {
    runArbitraryPathSpec
  }

  "sort arrays" in {
    import scalaz.Order
    import scalaz.Ordering._
 
    val v1 = JParser.parseUnsafe("""[1, 1, 1]""")
    val v2 = JParser.parseUnsafe("""[1, 1, 1]""")
 
    Order[JValue].order(v1, v2) must_== EQ
  }

  "sort objects by key" in {
    val v1 = JObject(
      JField("a", JNum(1)) ::
      JField("b", JNum(2)) ::
      JField("c", JNum(3)) :: Nil
    )

    val v2 = JObject(
      JField("b", JNum(2)) ::
      JField("c", JNum(3)) :: Nil
    )

    JValue.order(v1, v2) must_== LT
  }

  "sort objects by key then value" in {
    val v1 = JObject(
      JField("a", JNum(1)) ::
      JField("b", JNum(2)) ::
      JField("c", JNum(3)) :: Nil
    )

    val v2 = JObject(
      JField("a", JNum(2)) ::
      JField("b", JNum(3)) :: 
      JField("c", JNum(4)) :: Nil
    )
    
    JValue.order(v1, v2) must_== LT
  }

  "sort objects with undefined members" in {
    val v1 = JObject(
      JField("a", JUndefined) ::
      JField("b", JNum(2)) ::
      JField("c", JNum(3)) :: Nil
    )

    val v2 = JObject(
      JField("a", JNum(2)) ::
      JField("b", JNum(3)) :: 
      JField("c", JNum(4)) :: Nil
    )

    JValue.order(v1, v2) must_== GT
  }

  "Properly --> subclasses of JValue" in {
    val a = JNumStr("1.234")

    (a --> classOf[JNum]).isInstanceOf[JNum] mustEqual true
  }

  def runArbitraryPathSpec = {
    import org.scalacheck.Prop._

    implicit val arbJPath: Arbitrary[JPath] = Arbitrary {
      import Gen._

      val genIndex = for {
        index <- choose(0, 10)
      } yield JPathIndex(index)

      val genField = for {
        name <- identifier
      } yield JPathField(name)

      val genIndexOrField = Gen.frequency((1, genIndex), (9, genField))

      for {
        length      <- choose(0, 10)
        listOfNodes <- listOfN(length, genIndexOrField)
      } yield JPath(listOfNodes)
    }

    def badPath(jv: JValue, p: JPath): Boolean = {
      p.nodes match {
        case JPathIndex(index) :: xs => jv match {
          case JArray(nodes) => index > nodes.length || 
                                (index < nodes.length && badPath(nodes(index), JPath(xs))) ||
                                badPath(JArray(Nil), JPath(xs)) 

          case JObject(_) => true
          case _ => index != 0 || badPath(JArray(Nil), JPath(xs))
        }

        case JPathField(name) :: xs => jv match {
          case JArray(_) => true
          case _ => badPath(jv \ name, JPath(xs))
        }

        case Nil => false
      }
    } 

    val setProp = (jv: JValue, p: JPath, toSet: JValue) => {
      (!badPath(jv, p)) ==> {
        ((p == JPath.Identity) && (jv.set(p, toSet) == toSet)) ||
        (jv.set(p, toSet).get(p) == toSet)
      }
    }

    val insertProp = (jv: JValue, p: JPath, toSet: JValue) => {
      (!badPath(jv, p) && (jv(p) == JUndefined)) ==> {
        (jv, p.nodes) match {
          case (JObject(_), JPathField(_) :: _) | (JArray(_), JPathIndex(_) :: _) | (JNull | JUndefined, _) => 
            ((p == JPath.Identity) && (jv.unsafeInsert(p, toSet) == toSet)) ||
            (jv.unsafeInsert(p, toSet).get(p) == toSet)

          case _ => 
            jv.unsafeInsert(p, toSet) must throwA[RuntimeException]
        }
      }
    }

    check(setProp) and check(insertProp)
  }

  private def reorderFields(json: JValue) = json mapUp {
    case JObject(xs) => JObject(scala.collection.immutable.TreeMap(xs.toSeq: _*))
    case x => x
  }

  private def typePredicate(clazz: Class[_])(json: JValue) = json match {
    case x if x.getClass == clazz => true
    case _ => false
  }
}
