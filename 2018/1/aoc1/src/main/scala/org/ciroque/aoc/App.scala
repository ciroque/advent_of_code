package org.ciroque

import scala.annotation.tailrec

object AoC1 extends Data with App {
  println(s"Frequency Result: ${Solution.sum(frequencies)}")
  println(s"First Duplicate Frequency: ${Solution.firstDuplicateFrequency(frequencies)}")
}

object Solution {
  def sum(frequencies: List[Int]): Int = frequencies sum
  def firstDuplicateFrequency(frequencies: List[Int]): Int = {
    @tailrec
    def recurses(freqs: List[Int], sum: Int, sums: Set[Int]): Int = {
      freqs match {
        case _ if sums.contains(sum) =>
          sum
        case Nil =>
          recurses(frequencies, sum, sums) // restarts list
        case head :: tail =>
          val newSum = sum + head
          val newSums = sums + sum
          recurses(tail, newSum, newSums)
      }
    }

    recurses(frequencies, 0, Set())
  }
}

trait Data {
  lazy val frequencies: List[Int] = List(
    +3,
    +13,
    +4,
    -2,
    -4,
    +15,
    -1,
    +6,
    +3,
    -10,
    +9,
    -5,
    -7,
    -15,
    +8,
    -11,
    +19,
    +7,
    +6,
    +1,
    -13,
    +19,
    +7,
    +13,
    -18,
    +2,
    +6,
    +5,
    +8,
    +17,
    -11,
    +14,
    -10,
    -7,
    -9,
    -9,
    +11,
    +15,
    -4,
    +6,
    +13,
    -18,
    +7,
    +4,
    +16,
    -6,
    -7,
    +14,
    +3,
    -15,
    +1,
    -9,
    -21,
    -2,
    +9,
    -11,
    -8,
    -10,
    -11,
    +16,
    +8,
    -12,
    +8,
    +7,
    +1,
    +19,
    -5,
    -3,
    -29,
    -7,
    +11,
    +22,
    +14,
    -13,
    +19,
    +19,
    -6,
    +15,
    +20,
    +6,
    +9,
    +4,
    -9,
    -8,
    -19,
    +14,
    -5,
    +7,
    -8,
    +17,
    -4,
    -3,
    +6,
    +14,
    -16,
    -18,
    +1,
    -12,
    -4,
    -15,
    +24,
    +3,
    -5,
    +11,
    -16,
    +4,
    -14,
    -3,
    +6,
    +19,
    +3,
    -23,
    +19,
    -8,
    +21,
    +13,
    +11,
    +5,
    -18,
    -13,
    +21,
    -14,
    +9,
    +21,
    +12,
    -5,
    -16,
    +14,
    -13,
    +6,
    -18,
    -20,
    +27,
    +9,
    -6,
    +18,
    +8,
    -3,
    -3,
    -4,
    +8,
    +1,
    +7,
    +15,
    +13,
    -2,
    -4,
    -16,
    +7,
    -10,
    -7,
    +12,
    -6,
    -1,
    +13,
    +2,
    -6,
    +2,
    -7,
    +6,
    +14,
    -18,
    +2,
    +15,
    -3,
    -3,
    +5,
    +15,
    -4,
    +19,
    -14,
    +18,
    +1,
    -2,
    -5,
    -18,
    +10,
    -3,
    +15,
    +10,
    +17,
    +9,
    +5,
    +18,
    +1,
    +15,
    +9,
    -14,
    -3,
    +9,
    +6,
    -9,
    +5,
    +3,
    -10,
    -15,
    -12,
    +3,
    +14,
    -6,
    +17,
    -19,
    -1,
    -20,
    +1,
    +18,
    +7,
    +5,
    -9,
    -15,
    +4,
    +19,
    +6,
    +1,
    +14,
    +3,
    +3,
    +16,
    +12,
    +2,
    +13,
    +19,
    +13,
    -9,
    +11,
    +1,
    -8,
    +17,
    +2,
    +13,
    -7,
    -19,
    -19,
    -13,
    +19,
    -2,
    +12,
    -9,
    -12,
    +15,
    -9,
    -17,
    +15,
    -7,
    -12,
    +8,
    -6,
    -19,
    -7,
    -12,
    +6,
    -17,
    +22,
    +13,
    -6,
    +4,
    -1,
    +2,
    -18,
    -15,
    +18,
    -1,
    +13,
    +15,
    -19,
    -19,
    +4,
    -3,
    +11,
    +17,
    +6,
    +1,
    +4,
    +4,
    +17,
    -5,
    +15,
    +10,
    +4,
    +15,
    -13,
    +6,
    +9,
    -12,
    -20,
    +16,
    -12,
    -19,
    +12,
    +21,
    +6,
    -12,
    -20,
    -20,
    +10,
    -21,
    +3,
    -21,
    -15,
    -11,
    -8,
    -18,
    -2,
    +4,
    -5,
    +4,
    -15,
    +3,
    -2,
    -21,
    -6,
    +3,
    -17,
    +1,
    +4,
    +7,
    +19,
    -10,
    -8,
    +9,
    +13,
    +2,
    +11,
    -33,
    +9,
    +4,
    -9,
    +10,
    +1,
    +6,
    -18,
    -8,
    -24,
    +7,
    -23,
    +1,
    +11,
    +17,
    -19,
    +1,
    -13,
    +19,
    +16,
    -15,
    +7,
    -17,
    +5,
    +15,
    +13,
    +12,
    +9,
    -7,
    +1,
    -29,
    -5,
    +42,
    +10,
    +19,
    +20,
    +17,
    +13,
    -4,
    +1,
    +11,
    +5,
    +20,
    +23,
    +20,
    +17,
    +1,
    -3,
    -6,
    -6,
    +28,
    -24,
    -19,
    +31,
    -32,
    -46,
    +11,
    +9,
    -8,
    +1,
    +23,
    +34,
    +74,
    +20,
    +19,
    +1,
    -13,
    -34,
    +14,
    +6,
    +48,
    +18,
    +20,
    +5,
    -15,
    +6,
    -14,
    +17,
    -11,
    -24,
    +19,
    +15,
    -17,
    -21,
    +12,
    +2,
    +2,
    +2,
    +143,
    -1,
    -24,
    +52,
    +135,
    +8,
    -13,
    +30,
    +192,
    +72,
    -526,
    +60476,
    -14,
    +17,
    -16,
    +15,
    -9,
    +11,
    +13,
    -5,
    -6,
    +5,
    -4,
    -8,
    -10,
    +12,
    +19,
    +15,
    +16,
    +11,
    -15,
    +18,
    +14,
    +1,
    -10,
    +14,
    +11,
    +3,
    +13,
    -2,
    +12,
    +13,
    -8,
    -14,
    +7,
    -15,
    -5,
    +15,
    +18,
    +16,
    +16,
    +2,
    +5,
    -4,
    -5,
    -8,
    +14,
    +18,
    -9,
    +4,
    -16,
    -14,
    +8,
    +4,
    -5,
    +3,
    +11,
    +6,
    +19,
    +9,
    -18,
    +12,
    +10,
    +2,
    +1,
    +1,
    +19,
    +15,
    +1,
    -13,
    -13,
    +6,
    +9,
    -11,
    -12,
    -21,
    -19,
    +4,
    +10,
    +18,
    +12,
    +12,
    -1,
    +14,
    -2,
    +1,
    +18,
    +9,
    -15,
    -16,
    -14,
    +1,
    -5,
    -5,
    -7,
    -19,
    +8,
    +6,
    -5,
    +23,
    +13,
    -7,
    +12,
    +1,
    +6,
    +21,
    +14,
    +8,
    +17,
    +13,
    +13,
    +18,
    +16,
    -3,
    -18,
    -8,
    -1,
    -8,
    -1,
    -2,
    -18,
    -9,
    -8,
    +10,
    -5,
    +2,
    -18,
    -16,
    -4,
    -14,
    -12,
    -17,
    +20,
    +15,
    -1,
    +8,
    +7,
    +15,
    -12,
    -6,
    -9,
    +3,
    +14,
    +5,
    -4,
    +11,
    +4,
    +2,
    +11,
    +12,
    +11,
    -8,
    +10,
    -1,
    -19,
    +12,
    +1,
    -14,
    -2,
    +19,
    +15,
    +14,
    +3,
    -18,
    -3,
    +9,
    +3,
    -13,
    -4,
    +15,
    +1,
    +14,
    -3,
    +20,
    +13,
    -11,
    -4,
    -11,
    +2,
    +16,
    +1,
    +2,
    +13,
    +18,
    -12,
    -9,
    +2,
    +17,
    -13,
    -1,
    +9,
    -6,
    -14,
    +5,
    -6,
    -10,
    +3,
    -10,
    -6,
    -3,
    +7,
    +17,
    -6,
    -6,
    -22,
    -2,
    +9,
    -17,
    +13,
    +13,
    +4,
    -14,
    +5,
    -15,
    +13,
    -18,
    +8,
    +51,
    -3,
    +9,
    -17,
    +5,
    -4,
    -16,
    +29,
    +5,
    +1,
    +9,
    +19,
    -16,
    +8,
    +18,
    +9,
    +13,
    +8,
    -11,
    +8,
    +19,
    +6,
    -15,
    -12,
    +4,
    -11,
    -17,
    -6,
    -20,
    +17,
    +11,
    +17,
    -14,
    -10,
    -10,
    -18,
    -10,
    +15,
    -1,
    +15,
    +17,
    +13,
    +17,
    -3,
    +2,
    -1,
    +4,
    +19,
    +14,
    -16,
    +5,
    -10,
    -13,
    +15,
    +20,
    -10,
    -1,
    +19,
    -5,
    +4,
    -12,
    +20,
    +17,
    -15,
    +10,
    -18,
    +16,
    +12,
    +5,
    -12,
    +5,
    -4,
    +20,
    +1,
    +4,
    -3,
    -15,
    -18,
    +20,
    +7,
    -2,
    +12,
    +3,
    -1,
    -10,
    +15,
    +13,
    +17,
    +8,
    +1,
    +7,
    -11,
    -8,
    -8,
    -21,
    -19,
    -21,
    -9,
    -15,
    -14,
    -16,
    +7,
    -5,
    +22,
    +18,
    +14,
    -10,
    -21,
    +11,
    -30,
    +5,
    -6,
    +24,
    -26,
    -18,
    -9,
    +1,
    -5,
    -8,
    +14,
    -17,
    +9,
    -1,
    -2,
    -20,
    +6,
    +7,
    -5,
    +20,
    -2,
    +15,
    +25,
    +20,
    +28,
    +15,
    -32,
    -16,
    -2,
    +54,
    +15,
    +17,
    +18,
    -8,
    +2,
    +20,
    +6,
    +7,
    -10,
    -6,
    +30,
    -12,
    +36,
    +6,
    -4,
    +7,
    +9,
    +14,
    -9,
    -1,
    +14,
    -7,
    -10,
    +18,
    +17,
    +3,
    +1,
    -3,
    -17,
    +3,
    -11,
    +17,
    +3,
    +24,
    -11,
    +15,
    -25,
    +13,
    -6,
    -39,
    -10,
    +17,
    +11,
    -22,
    -28,
    +5,
    +3,
    -29,
    -23,
    -8,
    -19,
    +8,
    -22,
    -17,
    -16,
    +20,
    -18,
    +10,
    +41,
    -11,
    -202,
    -6,
    -14,
    +11,
    +34,
    -12,
    +487,
    +495,
    +60020,
    -18,
    +2,
    +7,
    +7,
    -8,
    -19,
    -16,
    +7,
    +10,
    -2,
    +19,
    -14,
    -15,
    -11,
    +16,
    +8,
    -16,
    -13,
    -6,
    +12,
    +18,
    +15,
    +4,
    -11,
    +20,
    -4,
    +3,
    +9,
    +5,
    -16,
    +6,
    -19,
    +17,
    +18,
    -2,
    -2,
    -9,
    +16,
    -15,
    +2,
    +19,
    +3,
    -10,
    -1,
    +19,
    -6,
    -8,
    -20,
    +19,
    +12,
    +16,
    -12,
    +15,
    -4,
    -7,
    +4,
    +1,
    -19,
    -1,
    -19,
    +1,
    -13,
    -17,
    -13,
    -17,
    -11,
    -19,
    -5,
    +9,
    -6,
    +18,
    +9,
    +4,
    -11,
    -1,
    -3,
    +8,
    +13,
    -9,
    +16,
    +20,
    -1,
    +10,
    -19,
    -6,
    +18,
    +5,
    -1,
    -18,
    -16,
    -11,
    +16,
    +2,
    -17,
    +4,
    +3,
    +18,
    -13,
    -25,
    -12,
    -17,
    -18,
    -10,
    -12,
    -15,
    +19,
    -14,
    +6,
    +10,
    +14,
    +16,
    -11,
    -2,
    -16,
    -13,
    -11,
    +9,
    +10,
    -18,
    -12,
    -6,
    -1,
    -13,
    +17,
    -6,
    +12,
    +2,
    +12,
    +9,
    +19,
    +20,
    -11,
    +5,
    +13,
    -9,
    -5,
    +3,
    -5,
    -15,
    -16,
    -4,
    +8,
    -14,
    -8,
    -8,
    +10,
    -7,
    -14,
    -12,
    -10,
    +8,
    +3,
    -19,
    +20,
    -15,
    -18,
    +22,
    +14,
    -6,
    -16,
    -2,
    -10,
    +9,
    -12,
    +4,
    -12,
    +10,
    -15,
    -18,
    -8,
    +9,
    +1,
    -11,
    +8,
    -121858
  )
}
