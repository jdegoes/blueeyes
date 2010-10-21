package blueeyes.util 

trait ProductPrefixUnmangler extends Product {
 def unmangledName = unmangleName(productPrefix)

 private def unmangleName(name: String): String = operators.foldLeft(name)((n, o) => n.replace(o._1, o._2))

 private val operators = Map(
   "$eq" -> "=", "$greater" -> ">", "$less" -> "<", "$plus" -> "+", "$minus" -> "-",
   "$times" -> "*", "$div" -> "/", "$bang" -> "!", "$at" -> "@", "$hash" -> "#",
   "$percent" -> "%", "$up" -> "^", "$amp" -> "&", "$tilde" -> "~", "$qmark" -> "?",
   "$bar" -> "|", "$bslash" -> "\\")
}

/*
trait HttpHeader extends ProductPrefixUnmangler {
 def value = unmangledName
}
*/

