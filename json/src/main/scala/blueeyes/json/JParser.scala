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

import scalaz.Validation
import scalaz.syntax.arrow._
import scalaz.std.function._

import Validation.fromTryCatch

import java.io.File
import java.nio.ByteBuffer

object JParser {

  // legacy parsing methods
  @deprecated("Use parseFromString() instead, which returns a Validation", "1.0")
  def parse(str: String): JValue = new StringParser(str).parse()

  type Result[A] = Validation[Throwable, A]
  type AsyncResult = (AsyncParse, AsyncParser)

  def parseFromString(str: String): Result[JValue] =
    fromTryCatch(new StringParser(str).parse())

  def parseFromFile(file: File): Result[JValue] =
    fromTryCatch(ChannelParser.fromFile(file).parse())

  def parseFromByteBuffer(buf: ByteBuffer): Result[JValue] =
    fromTryCatch(new ByteBufferParser(buf).parse())

  def parseManyFromString(str: String): Result[Seq[JValue]] =
    fromTryCatch(new StringParser(str).parseMany())

  def parseManyFromFile(file: File): Result[Seq[JValue]] =
    fromTryCatch(ChannelParser.fromFile(file).parseMany())

  def parseManyFromByteBuffer(buf: ByteBuffer): Result[Seq[JValue]] =
    fromTryCatch(new ByteBufferParser(buf).parseMany())

  def parseAsync(b: Option[ByteBuffer]): AsyncResult =
    AsyncParser().feed(b)

  def parseAsync(p: AsyncParser, b: Option[ByteBuffer]): AsyncResult =
    p.apply(b)
}
