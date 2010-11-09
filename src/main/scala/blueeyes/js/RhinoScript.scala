package blueeyes.js

import org.mozilla.javascript.{Context, ScriptableObject};

case class RhinoScript(script: String) extends Function[Map[String, _], Any]{
  def apply(parameters: Map[String, _]) = {
    val context = Context.enter();
    try{
      val scope = context.initStandardObjects();

      parameters.foreach(parameter => ScriptableObject.putProperty(scope, parameter._1, Context.javaToJS(parameter._2, scope)))

      context.evaluateString(scope, script, "javascript.log", 1, null);
    }
    finally{
      Context.exit();
    }
  }
}

trait RhinoScriptImplicits{
  def scriptToRhinoScript(script: String) = RhinoScript(script)
}

object RhinoScriptImplicits extends RhinoScriptImplicits
