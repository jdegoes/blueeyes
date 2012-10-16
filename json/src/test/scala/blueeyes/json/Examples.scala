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

import org.specs2.mutable.Specification

object Examples extends Specification {
  import JsonAST._
  import JsonDSL._
  import JsonParser._

  "Lotto example" in {
    val json = parse(lotto)
    val renderedLotto = compact(render(json))
    json mustEqual parse(renderedLotto)
  }

  "Person example" in {
    val json = parse(person)
    val renderedPerson = JsonDSL.pretty(render(json))
    json mustEqual parse(renderedPerson)
    render(json) mustEqual render(personDSL)
    compact(render(json \\ "name")) mustEqual """["Joe","Marilyn"]"""
    compact(render(json \ "person" \ "name")) mustEqual "\"Joe\""
  }

  "Transformation example" in {
    val uppercased = parse(person).transform(JField.liftCollect { case JField(n, v) => JField(n.toUpperCase, v) })
    val rendered = compact(render(uppercased))
    rendered mustEqual
      """{"PERSON":{"NAME":"Joe","AGE":35.0,"SPOUSE":{"PERSON":{"NAME":"Marilyn","AGE":33.0}}}}"""
  }

  "Remove example" in {
    val json = parse(person) remove { _ == JString("Marilyn") }
    compact(render(json \\ "name")) mustEqual "\"Joe\""
  }

  "Unbox values using XPath-like type expression" in {
    parse(objArray) \ "children" \\ classOf[JNum] mustEqual List(5, 3)
    parse(lotto) \ "lotto" \ "winning-numbers" \ classOf[JNum] mustEqual List(2, 45, 34, 23, 7, 5, 3)
    parse(lotto) \\ "winning-numbers" \ classOf[JNum] mustEqual List(2, 45, 34, 23, 7, 5, 3)
  }

  "Quoted example" in {
    val json = parse(quoted)
    List("foo \" \n \t \r bar") mustEqual json.values
  }

  "Null example" in {
    compact(render(parse(""" {"name": null} """))) mustEqual """{"name":null}"""
  }

  "Symbol example" in {
    compact(render(symbols)) mustEqual """{"f1":"foo","f2":"bar"}"""
  }

  "Unicode example" in {
    parse("[\" \\u00e4\\u00e4li\\u00f6t\"]") mustEqual JArray(List(JString(" \u00e4\u00e4li\u00f6t")))
  }

  "Exponent example" in {
    parse("""{"num": 2e5 }""") mustEqual JObject(List(JField("num", JNum(200000.0))))
    parse("""{"num": -2E5 }""") mustEqual JObject(List(JField("num", JNum(-200000.0))))
    parse("""{"num": 2.5e5 }""") mustEqual JObject(List(JField("num", JNum(250000.0))))
    parse("""{"num": 2.5e-5 }""") mustEqual JObject(List(JField("num", JNum(2.5e-5))))
  }

  "JSON building example" in {
    val json = concat(JObject(JField("name", JString("joe")) :: Nil), 
                      JObject(JField("age", JNum(34)) :: Nil)) ++ 
               concat(JObject(JField("name", JString("mazy")) :: Nil), 
                      JObject(JField("age", JNum(31)) :: Nil))

    compact(render(json)) mustEqual """[{"name":"joe"},{"age":34.0},{"name":"mazy"},{"age":31.0}]"""
  }

  "Example which collects all integers and forms a new JSON" in {
    val json = parse(person)
    val ints = json.foldDown(JNothing: JValue) { (a, v) => v match {
      case x: JNum => a ++ x
      case _ => a
    }}
    compact(render(ints)) mustEqual """[35.0,33.0]"""
  }

  "Example which folds up to form a flattened list" in {
    val json = parse(person)

    def form(list: JPath*): List[(JPath, JValue)] = list.toList.map { path =>
      (path, json(path))
    }

    val folded = (json.foldUpWithPath[List[(JPath, JValue)]](Nil) { (list, path, json) =>
      (path, json) :: list
    }).reverse.collect { case (p, j) => (p, j) }

    val formed = form(
      JPath("person.name"),
      JPath("person.age"),
      JPath("person.spouse.person.name"),
      JPath("person.spouse.person.age"),
      JPath("person.spouse.person"),
      JPath("person.spouse"),
      JPath("person"),
      JPath.Identity
    )

    folded mustEqual formed
  }

  "Renders JSON as Scala code" in {
    val json = parse(lotto)

    Printer.compact(renderScala(json)) mustEqual """JObject("lotto",JObject("lotto-id",JNum(5)::"winning-numbers",JArray(JNum(2)::JNum(45)::JNum(34)::JNum(23)::JNum(7)::JNum(5)::JNum(3)::Nil)::"winners",JArray(JObject("winner-id",JNum(23)::"numbers",JArray(JNum(2)::JNum(45)::JNum(34)::JNum(23)::JNum(3)::JNum(5)::Nil)::Nil)::JObject("winner-id",JNum(54)::"numbers",JArray(JNum(52)::JNum(3)::JNum(12)::JNum(11)::JNum(18)::JNum(22)::Nil)::Nil)::Nil)::Nil)::Nil)"""
  }

  val lotto = """
{
  "lotto":{
    "lotto-id":5,
    "winning-numbers":[2,45,34,23,7,5,3],
    "winners":[ {
      "winner-id":23,
      "numbers":[2,45,34,23,3, 5]
    },{
      "winner-id" : 54 ,
      "numbers":[ 52,3, 12,11,18,22 ]
    }]
  }
}
"""

  val person = """
{
  "person": {
    "name": "Joe",
    "age": 35.0,
    "spouse": {
      "person": {
        "name": "Marilyn",
        "age": 33.0
      }
    }
  }
}
"""

  val personDSL =
    ("person" ->
      ("name" -> "Joe") ~
      ("age" -> 35) ~
      ("spouse" ->
        ("person" ->
          ("name" -> "Marilyn") ~
          ("age" -> 33)
        )
      )
    )

  val objArray =
"""
{ "name": "joe",
  "address": {
    "street": "Bulevard",
    "city": "Helsinki"
  },
  "children": [
    {
      "name": "Mary",
      "age": 5.0
    },
    {
      "name": "Mazy",
      "age": 3.0
    }
  ]
}
"""

  val quoted = """["foo \" \n \t \r bar"]"""
  val symbols = ("f1" -> 'foo) ~ ("f2" -> 'bar)
}
