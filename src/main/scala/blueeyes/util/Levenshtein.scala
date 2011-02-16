package blueeyes.util

import scala.math.min

object Levenshtein {
  def levenshtein(str1: String, str2: String): Int = {
    def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)

    val lenStr1 = str1.length
    val lenStr2 = str2.length

    val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

    for (i <- 0 to lenStr1) d(i)(0) = i
    for (j <- 0 to lenStr2) d(0)(j) = j

    for (i <- 1 to lenStr1; val j <- 1 to lenStr2) {
      val cost = if (str1(i - 1) == str2(j - 1)) 0 else 1

      d(i)(j) = minimum(
        d(i - 1)(j) + 1, // deletion
        d(i)(j - 1) + 1, // insertion
        d(i - 1)(j - 1) + cost // substitution
        )
    }

    return d(lenStr1)(lenStr2)
  }

  def fuzzyEqual(str1: String, str2: String) = levenshtein(str1, str2) < 5
}