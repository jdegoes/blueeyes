package blueeyes.demo.xschema

import blueeyes.json.xschema._
import blueeyes.json.JsonAST.{JInt, JString}
import codegen.ScalaCodeGenerator
import blueeyes.json.JsonParser.{parse => j}

object XSchemaBootstrap{
  val demoPackage = "blueeyes.demo"

  val schemas = List(
    XRoot(
      List(
        XProduct(
          "Contact", demoPackage,
          Map(),
          List(
            XRealField("name",      Map(), XString, JString(""), XOrderIgnore),
            XRealField("email",     Map(), XOptional(XString), JString(""), XOrderIgnore),
            XRealField("country",   Map(), XOptional(XString), JString(""), XOrderIgnore),
            XRealField("city",      Map(), XOptional(XString), JString(""), XOrderIgnore),
            XRealField("address",   Map(), XOptional(XString), JString(""), XOrderIgnore)
          )
        )
      ),
      Nil,
      Map()
    )
  )
}

object Generator {
  def main(args: Array[String]) {
    def ff(path: String) = new java.io.FileWriter(path)
    XSchemaBootstrap.schemas.map(ScalaCodeGenerator.generator.generate(_, "src/main/scala", "src/test/scala", Nil, ff _))
  }
}
