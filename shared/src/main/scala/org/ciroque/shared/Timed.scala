package org.ciroque.shared

object Timing {
  def timed[R](block: => R): R = {
    val s1 = System.nanoTime()
    val result = block
    val e1 = System.nanoTime()
    println(s"Elapsed: ${e1 - s1}ns")
    result
  }
}
