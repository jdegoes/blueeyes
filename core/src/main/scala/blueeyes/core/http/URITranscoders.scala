package blueeyes.core.http

import blueeyes.util.SpecialCharToStringTranscoder

object URITranscoders{
  val SafePathChars  = "-_.!~*'()@:$&,;=/"
  val SafeQueryChars = "-_.!~*'()@:$,;/?:&="

  val pathTranscoder  = transcoder(SafePathChars)
  val queryTranscoder = transcoder(SafeQueryChars)

  private  def transcoder(safeChars: String) = new SpecialCharToStringTranscoder({
    case c: Char if !((c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || SafePathChars.indexOf(c) != -1) => {
      java.net.URLEncoder.encode(new String(Array(c)), "UTF-8")
    }
  }, {
    case c => None
  }){
    override def decode(s: String) = java.net.URLDecoder.decode(s, "UTF-8")
  }

}