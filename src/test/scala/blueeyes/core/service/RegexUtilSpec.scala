package blueeyes.core.service

import org.specs.Specification
import blueeyes.parsers.RegularExpressionAST._
import blueeyes.parsers.RegularExpressionPatten

class RegexUtilSpec extends  Specification with RegexUtil{
  "RegexUtil" should{
    "extract name with single named group" in{
      testExtract("(?<bar>:[a-z]+@)", ("(:[a-z]+@)", List("bar"), List("bar")))
    }
    "extract name without named group" in{
      testExtract("(?:foo)", ("(?:foo)", Nil, Nil))
    }
    "extract name with multiple named group" in{
      testExtract("((?<bar>:[a-z]+@)(?<foo>[1-9]+))", ("((:[a-z]+@)([1-9]+))", List("((?<bar>:[a-z]+@)(?<foo>[1-9]+))", "bar", "foo"), List("bar", "foo")))
    }
    "extract name from named group nested in Group" in{
      testExtract("(foo(?<bar>bar))", ("(foo(bar))", List("(foo(?<bar>bar))", "bar"), List("bar")))
    }
    "extract name from named group nested in NamedCaptureGroup" in{
      testExtract("(foo(?<bar>(?<foo>[1-9]+)))", ("(foo(([1-9]+)))", List("(foo(?<bar>(?<foo>[1-9]+)))", "bar", "foo"), List("bar", "foo")))
    }
    "extract name from named group nested in NonCapturingGroup" in{
      testExtract("(?:bar(?<foo>[1-9]+))", ("(?:bar([1-9]+))", List("foo"), List("foo")))
    }
    "extract name from named group nested in AtomicGroup" in{
      testExtract("(?>bar(?<foo>[1-9]+))", ("(?>bar([1-9]+))", List("foo"), List("foo")))
    }
    "extract name from named group nested in AtomicGroup and which contains group" in{
      testExtract("(?>bar(?<foo>([1-9])+))", ("(?>bar(([1-9])+))", List("foo", "([1-9])"), List("foo")))
    }
  }

  private def testExtract(input: String, output: (String, List[String], List[String])) = {
    extractNamedCaptureGroup(RegularExpressionPatten(input)) mustEqual (RegularExpressionPatten(output._1), output._2, output._3)
  }
}