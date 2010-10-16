/**
 * Copyright (c) 2010, Stefan Langer
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Element34 nor the names of its contributors may
 *       be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS ROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
package de.element34.sbteclipsify

/**
 * Base class for classpathentry filters.
 */
abstract class Filter(pattern: String) {
  def mkString = "=\"" + pattern + "\""
}
/**
 * Defines a include pattern for a classpath entry. <br />
 * e.g.: <code>&lt;classpathentry kind="src" path="src/" including="*.scala" /&gt;</code>
 */
case class IncludeFilter(pattern: String) extends Filter(pattern) {
  override def mkString = " including" + super.mkString
}
/**
 * Defines a exclude pattern for a classpath entry. <br />
 * e.g.: <code>&lt;classpathentry kind="src" path="src/" excluding="*.scala" /&gt;</code>
 */
case class ExcludeFilter(pattern: String) extends Filter(pattern) {
  override def mkString = " excluding" + super.mkString
}

/**
 * Combines <code>IncludeFilter</code> and <code>ExcludeFilter</code> for use in a <code>ClasspathEntry</code>
 */
case class FilterChain(inc: Option[IncludeFilter], ex: Option[ExcludeFilter]) {
  /**
   * Generates the actual markup for a classpathentry
   */
  def mkString: String = {
    def getStrOrEmpty[A <: Filter](opt: Option[A]) = {
      opt.map(_.mkString).getOrElse("")
    }
    getStrOrEmpty[IncludeFilter](inc) + getStrOrEmpty[ExcludeFilter](ex)
  }
}

/**
 * Companion object for <code>FilterChain</code> to provide convenience method for their creation.
 */
object FilterChain {
  def apply(inc: IncludeFilter, ex: ExcludeFilter) = new FilterChain(Some(inc), Some(ex))
  def apply(inc: IncludeFilter) = new FilterChain(Some(inc), None)
  def apply(ex: ExcludeFilter) = new FilterChain(None, Some(ex))
}

/**
 * Special type designating an empty <code>FilterChain</code>
 */
object EmptyFilter extends FilterChain(None, None)
