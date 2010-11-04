package blueeyes.util

import org.specs.Specification
import org.specs.util._

class CommandLineArgumentsSpec extends Specification {
  "No parameters or values should be parsed properly" in {
    val c = CommandLineArguments()
    
    c.parameters.size mustEqual(0)
    c.values.length mustEqual(0)
  }
  
  "Several parameters should be parsed properly" in {
    val c = CommandLineArguments("--foo", "bar", "--bar", "baz")
    
    c.parameters mustEqual Map(
      "foo" -> "bar",
      "bar" -> "baz"
    )
  }
  
  "Values combined with parameters should be parsed properly" in {
    val c = CommandLineArguments("baz", "--foo", "bar", "bar")
    
    c.parameters mustEqual Map("foo" -> "bar")
    c.values mustEqual List("baz", "bar")
  }
  
  "Parameters without values should have empty value strings" in {
    val c = CommandLineArguments("--baz", "--foo")
    
    c.parameters mustEqual Map("baz" -> "", "foo" -> "")
  }
  
  "Values combined with parameters should be counted properly" in {
    val c = CommandLineArguments("baz", "--foo", "bar", "bar")
    
    c.size mustEqual 3
  }
}