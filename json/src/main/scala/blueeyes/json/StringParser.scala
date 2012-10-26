package blueeyes.json

/**
 * Basic in-memory string parsing.
 *
 * This parser is limited to the maximum string size (~2G). Obviously for large
 * JSON documents it's better to avoid using this parser and go straight from
 * disk, to avoid having to load the whole thing into memory at once.
 */
private[json] final class StringParser(s: String)
extends SyncParser with CharBasedParser {
  var line = 0
  protected[this] final def column(i: Int) = i
  protected[this] final def newline(i: Int) { line += 1 }
  protected[this] final def reset(i: Int): Int = i
  protected[this] final def at(i: Int): Char = s.charAt(i)
  protected[this] final def at(i: Int, j: Int): String = s.substring(i, j)
  protected[this] final def atEof(i: Int) = i == s.length
  protected[this] final def close() = ()
}
