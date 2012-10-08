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
import Gen._
import Arbitrary.arbitrary

trait ArbitraryJValue {
  import JsonAST._

  def genJValue:  Gen[JValue]  = frequency((5, genSimple), (1, wrap(genArray)), (1, wrap(genObject)))
  def genJNum:    Gen[JNum]    = arbitrary[BigDecimal].map(JNum(_))
  def genJBool:   Gen[JBool]   = arbitrary[Boolean].map(JBool(_))
  def genJString: Gen[JString] = alphaStr.map(JString(_))
  def genSimple: Gen[JValue] = oneOf(
    value(JNull), 
    genJNum,
    genJBool,
    genJString)


  def genArray: Gen[JValue] = for (l <- genList) yield JArray(l)
  def genObject: Gen[JObject] = for (l <- genFieldList) yield JObject(l)

  def genList = Gen.containerOfN[List, JValue](listSize, genJValue)
  def genFieldList = Gen.containerOfN[List, JField](listSize, genField)
  def genField = for (name <- alphaStr; value <- genJValue; id <- choose(0, 1000000)) yield JField(name+id, value)

  def genJValueClass: Gen[Class[_ <: JValue]] = oneOf(
    JNull.getClass.asInstanceOf[Class[JValue]], 
    JNothing.getClass.asInstanceOf[Class[JValue]], 
    classOf[JNum], 
    classOf[JBool], 
    classOf[JString], 
    classOf[JArray], 
    classOf[JObject])

  def listSize = choose(0, 5).sample.get

  implicit def arbJValue: Arbitrary[JValue] = Arbitrary(genJValue)
  implicit def arbJObject: Arbitrary[JObject] = Arbitrary(genObject)
  implicit def arbJValueClass: Arbitrary[Class[_ <: JValue]] = Arbitrary(genJValueClass)
  implicit def shrinkJValueClass[T]: Shrink[T] = Shrink(x => Stream.empty)
  
  // BigDecimal *isn't* arbitrary precision!  AWESOME!!!
  implicit def arbBigDecimal: Arbitrary[BigDecimal] = Arbitrary(for {
    mantissa <- arbitrary[Long]
    exponent <- arbitrary[Int]
    
    adjusted = if (exponent.toLong + mantissa.toString.length >= Int.MaxValue.toLong)
      exponent - mantissa.toString.length
    else if (exponent.toLong - mantissa.toString.length <= Int.MinValue.toLong)
      exponent + mantissa.toString.length
    else
      exponent
  } yield BigDecimal(mantissa, adjusted, java.math.MathContext.UNLIMITED))
}
