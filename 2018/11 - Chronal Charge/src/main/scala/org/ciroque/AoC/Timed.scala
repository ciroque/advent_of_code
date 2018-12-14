package org.ciroque.shared

object Timing {
  def timed[R](method: String = "method")(block: => R): R = {
    val s1 = System.nanoTime()
    val result = block
    val e1 = System.nanoTime()
//    println(s"""{ "method": "$method", "timing": { ns: ${e1 - s1}, us: ${(e1 - s1) / 1000}, ms: ${(e1 - s1) / 1000 / 1000}, s: ${((e1 - s1) / 1000) / 1000 / 60} } }""")
    result
  }
}
