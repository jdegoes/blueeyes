package blueeyes.core.data

import blueeyes.json.JsonAST._
import blueeyes.json.Printer._
import blueeyes.json.JsonParser
import blueeyes.json.JsonParser.ParseException

trait BijectionsChunkJson{
  import java.io.{InputStreamReader, ByteArrayInputStream, OutputStreamWriter, ByteArrayOutputStream}

  implicit val JValueToChunk      = new Bijection[JValue, ByteChunk]{
    def apply(t: JValue)         = {
      val stream = new ByteArrayOutputStream()

      compact(render(t), new OutputStreamWriter(stream))

      new MemoryChunk(stream.toByteArray())
    }
    def unapply(s: ByteChunk)  = try {
      JsonParser.parse(new InputStreamReader(new ByteArrayInputStream(s.data)))
    }
    catch {
      case e: ParseException => throw new ParseException("""Data is too big, use big data handler. If "aggregate" combinator is used then probably Json is broken.""", e)
    }
  }

  implicit val ChunkToJValue    = JValueToChunk.inverse
}
object BijectionsChunkJson extends BijectionsChunkJson