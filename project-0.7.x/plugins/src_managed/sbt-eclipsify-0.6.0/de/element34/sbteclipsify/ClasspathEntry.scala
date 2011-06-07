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

/** Base class for classpathentry kinds */
abstract class Kind(val name: String)
/** defines the variable kind("var") for a classpathentry*/
case object Variable extends Kind("var")
/** defines the container kind ("con") for a classpathentry */
case object Container extends Kind("con")
/** defines the output kind ("output") for a classpathentry */
case object Output extends Kind("output")
/** defines the source kind ("src") for a classpathentry */
case object Source extends Kind("src")
/** defines the library kind ("lib") for a classpathentry */
case object Library extends Kind("lib")

/**
 * Defines a classpathentry in a .classpath file.
 * Each entry has a kind (either src, output, lib, var or con),
 * a path designating its location, a optional source path and
 * a include and exlucde filter as well as arbitrary attributes
 * that specify further information for the classpathentry.
 *
 * @see the eclipse documentatin for further information about classpathentries
 */
case class ClasspathEntry(kind: Kind, path: String, srcPath: Option[String], outputPath: Option[String], filter: FilterChain, attributes: List[Tuple2[String, String]]) {
  /** @see mkString(sep: String) */
  def mkString: String = mkString("")
  /**
   * converts this <code>ClasspathEntry</code > into a xml string representation
   * @param sep Defines the leading separater <code>String</code> prepended to each classpathentry
   */
  def mkString(sep: String): String = {
    sep +
    "<classpathentry kind=\"" + kind.name + "\"" +
    " path=\"" + path + "\"" +
    writeOptionalPath("sourcepath", srcPath) +
    writeOptionalPath("output", outputPath) +
    filter.mkString + (
	    if(attributes.isEmpty)
	    	" />"
	    else {
	    	def mkAttribute(item: Tuple2[String, String]) = {
	    	  "<attribute name=\"" + item._1 + "\" value=\"" + item._2 + "\" />"
	    	}
	    	val attrstr = ("" /: attributes.map(mkAttribute))(_ + _)
	    	">\n<attributes>\n"+  attrstr  + "\n</attributes>\n</classpathentry>"
	    }
    )
  }
  /** returns the sourcepath as a string when specified */
  def writeOptionalPath(attributeName: String, path: Option[String]): String = {
    path match {
      case Some(text) => " %s=\"%s\"".format(attributeName,text)
      case None => ""
    }
  }
}

/**
 * Factory providing convenience methods for creating <code>ClasspathEntry</code>
 */
object ClasspathEntry {
  def apply(kind: Kind, path: String) = new ClasspathEntry(kind, path, None, None, EmptyFilter, Nil)
  def apply(kind: Kind, path: String, srcPath: Option[String]) = new ClasspathEntry(kind, path, srcPath, None, EmptyFilter, Nil)
  def apply(kind: Kind, path: String, srcPath: String) = new ClasspathEntry(kind, path, Some(srcPath), None, EmptyFilter, Nil)
  def apply(kind: Kind, path: String, filter: FilterChain) = new ClasspathEntry(kind, path, None, None, filter, Nil)
  def apply(kind: Kind, path: String, outputPath: Option[String], filter: FilterChain) = new ClasspathEntry(kind, path, None, outputPath, filter, Nil)
  def apply(kind: Kind, path: String, outputPath: String, filter: FilterChain) = new ClasspathEntry(kind, path, None, Some(outputPath), filter, Nil)
  def apply(kind: Kind, path: String, attributes: List[Tuple2[String, String]]) = new ClasspathEntry(kind, path, None, None, EmptyFilter, attributes)
  def apply(kind: Kind, path: String, srcPath: String, attributes: List[Tuple2[String, String]]) = new ClasspathEntry(kind, path, Some(srcPath), None, EmptyFilter, attributes)
  def apply(kind: Kind, path: String, filter: FilterChain, attributes: List[Tuple2[String, String]]) = new ClasspathEntry(kind, path, None, None, filter, attributes)
  def apply(kind: Kind, path: String, srcPath: String, filter: FilterChain, attributes: List[Tuple2[String, String]]) = new ClasspathEntry(kind, path, Some(srcPath), None, filter, attributes)
}
