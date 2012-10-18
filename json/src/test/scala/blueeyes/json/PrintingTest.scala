///*
// * Copyright 2009-2010 WorldWide Conferencing, LLC
// *
// * Licensed under the Apache License, Version 2.0 (the "License");
// * you may not use this file except in compliance with the License.
// * You may obtain a copy of the License at
// *
// *     http://www.apache.org/licenses/LICENSE-2.0
// *
// * Unless required by applicable law or agreed to in writing, software
// * distributed under the License is distributed on an "AS IS" BASIS,
// * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// * See the License for the specific language governing permissions and
// * limitations under the License.
// */
//
//package blueeyes.json
//
//import org.scalacheck._
//import org.scalacheck.Prop.forAll
//import org.specs2.mutable.Specification
//import org.specs2.ScalaCheck
//
//object PrintingSpec extends Specification with ArbitraryJValue with ScalaCheck {
//  blueeyes.json.//  import scala.text.Document
//
//  "rendering does not change semantics" in {
//    def rendering = (json: Document) => parse(json.renderPretty) == parse(json.renderCompact)
//
//    check(rendering)
//  }
//
//  private def parse(json: String) = scala.util.parsing.json.JSON.parseRaw(json)
//
//  //implicit def arbDoc: Arbitrary[Document] = Arbitrary(genJValue.map(_.renderCompact))
//}
