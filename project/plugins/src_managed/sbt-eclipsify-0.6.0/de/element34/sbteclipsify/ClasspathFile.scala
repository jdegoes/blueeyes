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

import scala.xml._
import sbt._

import java.io.File
import java.nio.charset.Charset._

/** Implicit definitions for converting strings to paths and viceversa */
object ClasspathConversions {
	implicit def pathToString(path: Path): String = path.toString
	implicit def pathToOptionString(path: Path): Option[String] = Some(path.toString)
}


/**
 * Gathers the structural information for a .classpath file.
 * @param project The sbt project for which the .classpath file is created
 * @param log The logger from the sbt project
 */
class ClasspathFile(project: Project, log: Logger) {
	import ClasspathConversions._
	import Utils._

    lazy val classpathFile: File = project.info.projectPath / ".classpath" asFile
    val srcPatterns: List[String] = "-sources.jar" :: "-src.jar" :: Nil

    val scalaContainer = "org.scala-ide.sdt.launching.SCALA_CONTAINER"
    val javaContainer = "org.eclipse.jdt.launching.JRE_CONTAINER"
    val depPluginsContainer = "org.eclipse.pde.core.requiredPlugins"

    implicit val p = project

    /**
     * writes the .classpath file to the project root
     * @return <code>Some(error)</code>, where error designates the error message to display, when an error occures else returns <code>None</code>
     */
    def writeFile: Option[String] = {
    	val basicScalaPaths = project.asInstanceOf[BasicScalaPaths]
    	val dependencies = basicScalaPaths.dependencyPath
    	val managedDependencies = basicScalaPaths.managedDependencyPath

    	val entries = getJavaPaths ++ getScalaPaths ++ getProjectPath ++ getSbtJarForSbtProject ++
	      			  getDependencyEntries(dependencies) ++ getDependencyEntries(managedDependencies) ++
	      			  getPluginEntries ++
	      			  List(ClasspathEntry(Container, scalaContainer),
	      			  ClasspathEntry(Container, javaContainer),
	      			  ClasspathEntry(Output, project.asInstanceOf[MavenStyleScalaPaths].mainCompilePath.projectRelativePath))

	    lazy val classpathContent = """<?xml version="1.0" encoding="UTF-8" ?>""" +
	    	"\n<classpath>" +
	    	("" /: entries)(_ + _.mkString("\n")) +
	    	"\n</classpath>"
	    createOrReplaceWith(classpathContent)
  	}

	/**
	 * replaces the current content of the .classpath file
	 * @return <code>Some(error)</code> when error occurs else returns <code>None</code>
     */
	def createOrReplaceWith(content: String): Option[String]= {
		FileUtilities.touch(classpathFile, log) match {
			case Some(error) =>
				Some("Unable to write classpath file " + classpathFile + ": " + error)
			case None =>
				FileUtilities.write(classpathFile, content, forName("UTF-8"), log)
		}
	}

	/**
     * @return <code>List[ClasspathEntry]</code> containing entries for each jar contained in path.
     */
	def getDependencyEntries(basePath: Path): List[ClasspathEntry] = {
		import Path._

		val exclude: List[PathFinder] = constructPathFinder(basePath, srcPatterns, str => GlobFilter("*" + str))
		val baseFinder: PathFinder = basePath ** GlobFilter("*.jar")
		val finder: PathFinder = exclude.foldLeft(baseFinder)(_ --- _)

		val jarPaths: List[Path] = finder.get.flatMap(Path.relativize(project.info.projectPath, _)).toList
		jarPaths.map(path => ClasspathEntry(Library, path.relativePath, findSource(basePath, path)))
	}

	private def findSource(basePath: Path, jar: Path): Option[String] = {
		import sbt.Project._
		val JarEx = """.*/([^/]*)\.jar""".r
		jar.toString match {
			case JarEx(name) => {
				val finders: List[PathFinder] = constructPathFinder(basePath, srcPatterns, str => new ExactFilter(name + str))
				val seq = finders.foldLeft(Path.emptyPathFinder)(_ +++ _).get.toSeq
				seq.firstOption.map(_.toString)
			}
			case _ => None
		}
	}

	private def constructPathFinder(basePath: Path, list: List[String], conv: String => FileFilter): List[PathFinder] = {
		list.map(str => basePath ** conv(str))
	}

	/**
     * @return <code>List[ClasspathEntry]</code> with entries for the project build directory and the project plugin directory
     */
  	def getProjectPath: List[ClasspathEntry] = {
	    val entries: List[ClasspathEntry] = if(get(_.includeProject) && project.info.builderProjectPath.exists) {
	    	ClasspathEntry(Source, project.info.builderProjectPath, FilterChain(IncludeFilter("**/*.scala"))) :: Nil
	    } else Nil

	    if(get(_.includePlugin) && project.info.pluginsPath.exists) {
	    	ClasspathEntry(Source, project.info.pluginsPath, FilterChain(IncludeFilter("**/*.scala"))) :: entries
	    } else entries
	}

    /**
     * @return <code>List[ClasspathEntry]</code> with entries for the main source and main test source path.
     */
	def getScalaPaths: List[ClasspathEntry] = {
		import ClasspathConversions._
	    val paths = project.asInstanceOf[MavenStyleScalaPaths]
	    val entries: List[ClasspathEntry] = if(paths.mainScalaSourcePath.exists) {
	    	ClasspathEntry(Source, paths.mainScalaSourcePath.projectRelativePath, FilterChain(IncludeFilter("**/*.scala"))) :: Nil
	    } else Nil

	    if(paths.testScalaSourcePath.exists) {
	    	ClasspathEntry(Source, paths.testScalaSourcePath.projectRelativePath, paths.testCompilePath.projectRelativePath, FilterChain(IncludeFilter("**/*.scala"))) :: entries
	    } else entries
	}

    /**
     * @return <code>List[ClasspathEntry]</code> for main java source and main java test source path
     */
	def getJavaPaths: List[ClasspathEntry] = {
	    import ClasspathConversions._
		val paths = project.asInstanceOf[MavenStyleScalaPaths]
	    val entries: List[ClasspathEntry] = if (paths.testJavaSourcePath.exists) {
	    	ClasspathEntry(Source, paths.testJavaSourcePath.projectRelativePath, FilterChain(IncludeFilter("**/*.java"))) :: Nil
	    } else Nil

	    if (paths.mainJavaSourcePath.exists) {
	    	ClasspathEntry(Source, paths.mainJavaSourcePath.projectRelativePath, paths.testCompilePath.projectRelativePath, FilterChain(IncludeFilter("**/*.java"))) :: entries
	    } else entries
	}


	/**
     * @return <code>List[ClasspathEntry]</code> for sbt jar
     */
	def getSbtJarForSbtProject: List[ClasspathEntry] = {
	    val plugin = project.asInstanceOf[Eclipsify]
	    if(plugin.sbtDependency.value || plugin.includeProject.value || plugin.includePlugin.value) {
			val scalaVersion = project.buildScalaVersion
		    val sbtVersion = project.sbtVersion.get.get
		    // TODO how to handle cross builds?
		    val sbtJar = "sbt_" + scalaVersion + "-" + sbtVersion + ".jar"
		    val foundPaths = project.info.projectPath / "project" / "boot" ** new ExactFilter(sbtJar) get
		    val entries: List[ClasspathEntry] = foundPaths.map(ClasspathEntry(Library, _)).toList
		    entries
		}
	    else Nil
	}

	def getPluginEntries: List[ClasspathEntry] = {
		val plugin = project.asInstanceOf[Eclipsify]
		if(plugin.pluginProject.value)
			List(ClasspathEntry(Container, depPluginsContainer))
		else Nil
	}
}

/**
 * Factory for creating ClasspathFile instances
 */
object ClasspathFile {

	def apply(project: Project, log: Logger) = new ClasspathFile(project, log)
}

