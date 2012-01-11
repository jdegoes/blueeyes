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
import org.specs2.ScalaCheck

object XmlSpec extends Specification with ArbitraryXml with ArbitraryJValue with ScalaCheck {
  import Xml._
  import JsonAST._
  import Printer._
  import JsonParser.parse
  import scala.xml.Node

  "Valid XML can be converted to JSON and back (symmetric op)" in {
    val conversion = (xml: Node) => { toXml(toJson(xml)).text == xml.text }
    check(conversion)
  }

  "JSON can be converted to XML, and back to valid JSON (non symmetric op)" in {
    val conversion = (json: JValue) => { parse(compact(render(toJson(toXml(json))))); true }
    check(conversion)
  }
}
