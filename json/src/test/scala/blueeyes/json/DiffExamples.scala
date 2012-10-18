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

package blueeyes
package json

import org.specs2.mutable.Specification

object DiffExamples extends Specification {
  import JsonAST._
  import JsonParser._
  import MergeExamples.{scala1, scala2, lotto1, lotto2, mergedLottoResult}
  
  "Diff example" in {
    val Diff(changed, added, deleted) = scala1 diff scala2
    changed mustEqual expectedChanges
    added mustEqual expectedAdditions
    deleted mustEqual expectedDeletions
  }
  
  val expectedChanges = parse("""
    {
      "tags": ["static-typing","fp"],
      "features": {
        "key2":"newval2"
      }
    }""")
  
  val expectedAdditions = parse("""
    {
      "features": {
        "key3":"val3"
      },
      "compiled": true
    }""")
  
  val expectedDeletions = parse("""
    {
      "year":2006,
      "features":{ "key1":"val1" }
    }""")
  
  "Lotto example" in {
    val Diff(changed, added, deleted) = mergedLottoResult diff lotto1
    changed mustEqual JNothing
    added mustEqual JNothing
    deleted mustEqual lotto2
  }
  
  "Example from http://tlrobinson.net/projects/js/jsondiff/" in {
    val json1 = read("/diff-example-json1.json")
    val json2 = read("/diff-example-json2.json")
    val expectedChanges = read("/diff-example-expected-changes.json")
    val expectedAdditions = read("/diff-example-expected-additions.json")
    val expectedDeletions = read("/diff-example-expected-deletions.json")
  
    val Diff(changes, additions, deletions) = json1 diff json2
    changes.renderCanonical mustEqual expectedChanges.renderCanonical
    additions.renderCanonical mustEqual expectedAdditions.renderCanonical
    deletions.renderCanonical mustEqual expectedDeletions.renderCanonical
  }
  
  private def read(resource: String) = parse(scala.io.Source.fromInputStream(getClass.getResourceAsStream(resource)).getLines.mkString)
}
