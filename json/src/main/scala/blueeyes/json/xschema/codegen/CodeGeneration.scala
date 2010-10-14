package blueeyes.json.xschema.codegen

import _root_.blueeyes.json.JsonDSL._
import _root_.blueeyes.json.JsonAST._
import _root_.blueeyes.json.JsonParser
import _root_.blueeyes.json.xschema.{XRoot}
import _root_.blueeyes.json.xschema.Serialization._

import java.lang.StringBuilder
import java.io._

import scala.collection.mutable.{Map => MutableMap}

class State(var indentLevel: Int) {
  def indent   = { indentLevel = indentLevel + 1; this }
  def unindent = { indentLevel = indentLevel - 1; this }
  
  def replaceWith(that: State) = { this.indentLevel = that.indentLevel; this }
  
  def tab = "  "
  
  def startIndentation = (0 until indentLevel).foldLeft("") { (cur, l) => cur + tab }
  
  def column = tab.length * indentLevel
}

object State {
  def apply(indentLevel: Int) = new State(indentLevel)
}

case class CodeBuilder(codeBuilder: StringBuilder, state: State) {
  import scala.collection.mutable.ArrayStack
  import scala.util.matching.Regex
  
  private val Indented = """^( +)(.*)$""".r
  
  private val replacements = new ArrayStack[List[(String, String)]]
  
  private var row = 0
  private var col = 0
  
  def += (str: String): CodeBuilder = { 
    def replace(template: String, replacements: Iterable[(String, String)]) = replacements.foldLeft(template) { (t, r) => t.replace("${" + r._1 + "}", r._2) }
    
    var newStr = replacements.toList.foldLeft(str) { (str, replacement) => replace(str, replacement) }
    
    var lines = newStr.split("\n").toList match {
      case x :: xs if (x.trim.length == 0) => xs
      case xs => xs
    }
    
    if (lines.length > 1) {
      // Multiline:
      var isFirst = true
    
      var indents = new ArrayStack[Int]
    
      var startIndentLevel = state.indentLevel
    
      lines.foreach { line =>
        var strippedLine = line match {
          case Indented(spaces, rest) if (lines.length > 1) =>
            val count = spaces.length
          
            if (indents.size == 0) {
              indents.push(count)
            }
            else {
              val lastCount = indents.top
            
              if (count != lastCount) {
                if (count > lastCount) {
                  state.indent
                
                  indents.push(count)
                }
                else if (count < lastCount) {
                  while (indents.size > 0 && indents.top != count) {
                    indents.pop
                  
                    state.unindent
                  }
                }
              }
            }

            rest
          
          case _ => line
        }
    
        if (isFirst) isFirst = false else newline
      
        append(strippedLine)
      }
    
      state.indentLevel = startIndentLevel
    }
    else {
      // Single line
      append(newStr);
    }
    
    this
  }
  
  def += (that: CodeBuilder): CodeBuilder = { 
    this.append(that.code)
    this.state.replaceWith(that.state)
    
    this
  }
  
  def addln(template: String, replacements: (String, String)*) = add(template, replacements: _*).newline
  
  def add(template: String, replacements: (String, String)*): CodeBuilder = using[CodeBuilder](replacements: _*) { this += template }
  
  def using[T](replacements: (String, String)*)(f: => T): T = {
    this.replacements.push(replacements.toList)
    
    val returnValue = f
    
    this.replacements.pop
    
    returnValue
  }
  
  def indent(f: => Unit): CodeBuilder = {
    indent
    
    f
    
    unindent
  }
  
  def block(f: => Unit): CodeBuilder = block(f, "{", "}")
  
  def paren(f: => Unit): CodeBuilder = block(f, "(", ")")
  
  def block(f: => Unit, begin: String, end: String): CodeBuilder = {
    add(begin).indent
    
    f
    
    unindent.add(end)
  }
  
  def wrap(str: String, linePrefix: String, limit: Int): CodeBuilder = {
    val words = str.split(" +")
    
    var isFirst = true
    
    for (i <- 0 until words.length) {
      if (isFirst) isFirst = false else add(" ")
      
      add(words(i))
      
      var peekLength = if (i < words.length - 1) words(i + 1).length + 1 else 0
      
      if (col + peekLength > limit) {
        newline.add(linePrefix)
        
        isFirst = true
      }
    }
    
    this
  }
  
  def apply(f: => String): CodeBuilder = add(f)
  
  def indent   = { state.indent; newline }
  def unindent = { state.unindent; newline }
  def newline  = append("\n").append(state.startIndentation)
  
  def newline(n: Int): CodeBuilder = { (0 until n) foreach { x => newline }; this }
  
  def join[T](iterable: Iterable[T], joiner: => Unit)(f: T => Unit): CodeBuilder = {
    var isFirst = true
    
    for (element <- iterable) {
      if (isFirst) isFirst = false else joiner
      
      f(element)
    }
    
    this
  }
  
  def code = codeBuilder.toString
  
  private def append(str: String): CodeBuilder = {
    for (i <- 0 until str.length) {
      if (str.charAt(i) == '\n') {
        row += 1
        col = 0
      }
      else {
        col += 1
      }
    }
    
    codeBuilder.append(str)
    
    this
  }
}

object CodeBuilder {
  def empty = new CodeBuilder(new StringBuilder(), State(0))
}

case class CodeBundle(fileToCG: MutableMap[String, CodeBuilder]) {
  def += (tuple: (String, CodeBuilder)) = {
    val file  = tuple._1
    val oldCG = forFile(file)
    val newCG = tuple._2
    
    fileToCG += file -> (oldCG += newCG)
  }
  
  def create(root: String, writerF: String => Writer) = {
    for ((file, cg) <- fileToCG) {
      val absPath = root + "/" + file
      
      val os = writerF(absPath)
      
      try {
        os.write(cg.code)
        os.flush
      }
      finally {
        os.close();
      }
    }
  }
  
  private def forFile(file: String) = if (fileToCG.contains(file)) fileToCG(file) else {
    fileToCG += file -> CodeBuilder.empty
    
    fileToCG(file)
  }
}

object CodeBundle {
  def empty = new CodeBundle(MutableMap())
}

trait CodeGeneratorHelpers {
  def toDirectory(ns: String) = ns.replace(".", "/") + "/"

  def toFile(ns: String, name: String, extension: String): String = toDirectory(ns) + name + "." + extension
}

object CodeGenerator {
  def using[T <: Closeable, S](c: => T)(f: T => S): S = { val resource = c; try { f(resource) } finally { resource.close } }

  def writer(s: String) = {
    val outFile = new File(s)
    outFile.getParentFile.mkdirs
    new FileWriter(outFile)
  }
}

abstract class CodeGenerator {
  import CodeGenerator._
  def generate(root: XRoot, destCodePath: String, destTestsPath: String, namespaces: List[String], writerF: String => Writer)

  def generateFromFiles(xschemaFiles: Array[String], destCodePath: String, destTestsPath: String, namespaces: Array[String]) {
    def using[T <: Closeable, S](c: => T)(f: T => S): S = { val resource = c; try { f(resource) } finally { resource.close } }
    def load(file: String): XRoot = using(new InputStreamReader(new FileInputStream(file))) {
      r => JsonParser.parse(r).deserialize[XRoot]
    }
        
    val root = xschemaFiles.map(load).foldLeft(XRoot(Nil, Nil, Map())) { (accum, part) =>
      XRoot(accum.definitions ++ part.definitions, accum.constants ++ part.constants, accum.properties ++ part.properties)
    }

    generate(root, destCodePath, destTestsPath, namespaces.toList, writer _)
  }

  def generateXSchema(root: XRoot, destSchemaPath: String) {
    using(new PrintWriter(writer(destSchemaPath))) {
      out => out.print(pretty(render(root.serialize))) 
    }
  }
}

trait CodeGeneratorCLI {
  val generator: CodeGenerator

  def main(args: Array[String]) {
    import java.io._
    
    try {
      var parsed = parseArgs(args.toList)
      
      val codeDir      = parsed.config.get("codeDir")
      val testsDir     = parsed.config.get("testsDir")
      val xschemaFiles = parsed.elements
      val namespace    = parsed.config.get("namespace")
      
      val valid = !codeDir.isEmpty && !testsDir.isEmpty && xschemaFiles.length > 0
      
      if (args.length < 3) {
        println("Usage: --codeDir [code directory] --testsDir [tests directory] [XSchema files] [--namespace [namespace]]")
      }
      else {
        val destCodePath  = codeDir.get
        val destTestsPath = testsDir.get
       
        generator.generateFromFiles(xschemaFiles.toArray, destCodePath, destTestsPath, namespace.toList.toArray)
        println("Successfully generated code at " + destCodePath + " and tests at " + destTestsPath)
        
        System.exit(0)
      }
    }
    catch {
      case t: Throwable => 
        println("Encountered error during code generation: " + t.getMessage)
        
        System.exit(1)
    }
  }
  
  case class ParsedOptions(elements: List[String], flags: List[String], config: Map[String, String]) {
    def + (that: ParsedOptions): ParsedOptions = ParsedOptions(elements ++ that.elements, flags ++ that.flags, config ++ that.config)
  }
  
  def parseArgs(args: List[String]): ParsedOptions = {
    import scala.util.matching.Regex
    
    val parsed = ParsedOptions(Nil, Nil, Map())
    
    val Key   = """-+([a-zA-Z0-9_\-]+)""".r
    val Value = """([^-]+)""".r
    
    args match {
      case Key(key) :: Value(value) :: rest => ParsedOptions(parsed.elements, parsed.flags, parsed.config + (key -> value)) + parseArgs(rest)
      case Key(key) :: rest                 => ParsedOptions(parsed.elements, parsed.flags ++ List(key), parsed.config) + parseArgs(rest)
      case element :: rest                  => ParsedOptions(parsed.elements ++ List(element), parsed.flags, parsed.config) + parseArgs(rest)
      
      case Nil => parsed
    }
  }
}