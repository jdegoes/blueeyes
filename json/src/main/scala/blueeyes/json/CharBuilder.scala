package blueeyes.json

// efficient char-by-char string builder, taken from jawn under MIT license.
// (https://github.com/non/jawn)

final class CharBuilder {
  @inline final def INITIALSIZE = 16

  private var cs = new Array[Char](INITIALSIZE)
  private var capacity = INITIALSIZE
  private var len = 0

  def makeString: String = new String(cs, 0, len)

  def extend(s: String) {
    var i = 0
    val len = s.length
    while (i < len) { append(s.charAt(i)); i += 1 }
  }

  def append(c: Char) {
    if (len == capacity) {
      val n = capacity * 2
      val ncs = new Array[Char](n)
      System.arraycopy(cs, 0, ncs, 0, capacity)
      cs = ncs
      capacity = n
    }
    cs(len) = c
    len += 1
  }
}
