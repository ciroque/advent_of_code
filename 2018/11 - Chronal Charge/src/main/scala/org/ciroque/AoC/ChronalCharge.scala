package org.ciroque.optimized

import Timing._

object ChronalCharge extends Data with App {
  val m = timed("createPowerMatrix") { PuzzleSolution.createPowerMatrix(gridSerialNumber, gridSize) }
  val n = timed("createPowerMatrix2") { PuzzleSolution.createPowerMatrix2(gridSerialNumber, gridSize)}.toArray

  val mSat = timed("SummedAreaTable.generateSummedAreaTableRecursive") { SummedAreaTable.generateSummedAreaTableRecursive(gridSize, m) }
  val nSat = timed("SummedAreaTable.generateSummedAreaTableIterative") { SummedAreaTable.generateSummedAreaTableIterative(gridSize, n) }

  println(s"Array: ${m.toList diff n.toList}")
  println(s"Array: ${mSat.toList diff nSat.toList}")

  println("Actual Part One solution: FuelCell(6621,Coordinate(21,22),34)")
  println(s"partOne: ${ timed("partOne") { PuzzleSolution.partOne(gridSerialNumber, gridSize)}}")


  println(s"Wikipedia Grid: ${ timed("Wikipedia Grid") { PuzzleSolution.wikipediaVerification() } }")
  println(s"ATI Grid: ${ timed("Wikipedia Grid") { PuzzleSolution.atiVerification() } }")
}

object PuzzleSolution {

  def atiVerification(): List[Int] = {
    val atiGrid = List(2, 3, 2, 1, 3, 0, 1, 2, 1, 3, 1, 0, 1, 4, 2, 2).toArray
    SummedAreaTable.generateSummedAreaTableIterative(4, atiGrid).toList
  }

  def wikipediaVerification(): List[Int] = {
    val wikipediaGrid = List(31, 2, 4, 33, 5, 36, 12, 26, 9, 10, 29, 25, 13, 17, 21, 22, 20, 18, 24, 23, 15, 16, 14, 19, 30, 8, 28, 27, 11, 7, 1, 35, 34, 3, 32, 6).toArray
    SummedAreaTable.generateSummedAreaTableIterative(6, wikipediaGrid).toList
  }

  def partOne(gridSerialNumber: Int, gridSize: Int): (Int, Int) = {
    val powerMatrix = createPowerMatrix2(gridSerialNumber, gridSize)
    val summedAreaTable = SummedAreaTable.generateSummedAreaTableRecursive(gridSize, powerMatrix)

    val x = 21
    val y = 22

    val topLeft = (y * gridSize) + x                    // A
    val topRight = (y * gridSize) + (x + 2)             // B
    val bottomLeft = ((y + 2) * gridSize) + x           // C
    val bottomRight = ((y + 2) * gridSize) + (x + 2)    // D
//
//    val topLeft = ((y - 1) * gridSize) + (x - 1)        // A
//    val topRight = ((y - 1) * gridSize) + (x + 2)       // B
//    val bottomLeft = ((y + 2) * gridSize) + (x - 1)     // C
//    val bottomRight = ((y + 2) * gridSize) + (x + 2)    // D

    val A = summedAreaTable(topLeft)
    val B = summedAreaTable(topRight)
    val C = summedAreaTable(bottomLeft)
    val D = summedAreaTable(bottomRight)

    // D - B - C + A
    val sum = D + A - B - C

    println(
      s"""
         | topLeft: $topLeft
         | A = $A
         | B = $B
         | C = $C
         | D = $D
         | sum = $sum
         |
       """.stripMargin)

    (-1, -1)
  }

  def createPowerMatrix(gridSerialNumber: Int, gridSize: Int): Array[Int] = {
    def calculatePowerLevel(x: Int, y: Int): Int = (((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) % 10) - 5

    val length = gridSize * gridSize
    val powerMatrix: Array[Int] = timed("create array") { Array.ofDim[Int](length) }

    (0 until length).foreach {
      index => powerMatrix(index) = calculatePowerLevel((index % gridSize), (index / gridSize))
    }

    powerMatrix
  }

  def createPowerMatrix2(gridSerialNumber: Int, gridSize: Int): Array[Int] = {
    def calculatePowerLevel(x: Int, y: Int): Int = (((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) % 10) - 5

    val coreList = (for(
      i <- 0 until (gridSize * gridSize);
      x = i % gridSize;
      y = i / gridSize
    ) yield calculatePowerLevel(x, y)).toList

    val baseIndex = gridSize * gridSize

    val xMaxList = (0 until gridSize).map(index => calculatePowerLevel(index, 300)).toList
    val yMaxList = (0 until gridSize).map(index => calculatePowerLevel(300, index)).toList

    (coreList ++ xMaxList ++ yMaxList).toArray
  }
}

object SummedAreaTable {

  /**
    * Calculates the Summed Area Table for the given Square.
    *
    * This implementation represents the square as a single-dimension array of integers.
    *
    * This algorithm uses a two-pass approach to calculating the sums with a recursive algorithm.
    *
    * Generally this will run in O(2n) time.
    *
    * @param gridSize: Int - The length of a side of the square
    * @param grid: Array[Int] - The array for which the Summed Area Table will be calculated
    * @return: Array[Int] - The Summed Area Table
    */
  def generateSummedAreaTableRecursive(gridSize: Int, grid: Array[Int]): Array[Int] = {
    // Sums the rows
    @scala.annotation.tailrec
    def recursesX(size: Int, currentIndex: Int, grid: Array[Int], tableSoFar: Array[Int]): Array[Int] = {
      if (currentIndex == size) tableSoFar
      else {
        val prevIndex = currentIndex - 1
        val nextValue = if (prevIndex < 0) 0 else tableSoFar(prevIndex)
        val multiplier = Math.min(currentIndex % gridSize, 1) // captures start of next row
        tableSoFar(currentIndex) = grid(currentIndex) + (nextValue * multiplier)

        recursesX(size, currentIndex + 1, grid, tableSoFar)
      }
    }

    // sums the columns
    @scala.annotation.tailrec
    def recursesY(size: Int, currentIndex: Int, sat: Array[Int]): Array[Int] = {
      if (currentIndex == size) sat
      else {
        val currentValue = sat(currentIndex)
        val aboveIndex = currentIndex - gridSize
        val aboveValue = if (aboveIndex < 0) 0 else sat(aboveIndex)
        sat(currentIndex) = currentValue + aboveValue

        recursesY(size, currentIndex + 1, sat)
      }
    }

    val xSums = recursesX(gridSize * gridSize, 0, grid, Array.ofDim[Int](gridSize * gridSize))

    recursesY(gridSize * gridSize, 0, xSums)
  }

  /**
    * Calculates the Summed Area Table for the given Square.
    *
    * This implementation represents the square as a single-dimension array of integers.
    *
    * This algorithm uses a two-pass approach to calculating the sums with two loops.
    *
    * Generally this will run in O(2n) time.
    *
    * @param gridSize: Int - The length of a side of the square
    * @param grid: Array[Int] - The array for which the Summed Area Table will be calculated
    * @return: Array[Int] - The Summed Area Table
    */
  def generateSummedAreaTableIterative(gridSize: Int, grid: Array[Int]): Array[Int] = {
    val summedAreaTable = Array.ofDim[Int](gridSize * gridSize)
    val size = gridSize * gridSize
    // Sums the rows

    (0 until size).foreach {
      i =>
        val prevIndex = i - 1
        val nextValue = if (prevIndex < 0) 0 else summedAreaTable(prevIndex)
        val multiplier = Math.min(i % gridSize, 1) // captures start of next row
        summedAreaTable(i) = grid(i) + (nextValue * multiplier)
    }

    // sums the columns
    (0 until size).foreach {
      i =>
        val currentValue = summedAreaTable(i)
        val aboveIndex = i - gridSize
        val aboveValue = if (aboveIndex < 0) 0 else summedAreaTable(aboveIndex)
        summedAreaTable(i) = currentValue + aboveValue
    }

    summedAreaTable
  }

  /**
    * Calculates the Summed Area Table for the given Square.
    *
    * This implementation represents the square as a two-dimension array of integers.
    *
    * This algorithm uses a nested loop approach, easy to read and understand.
    *
    * Generally this will run in O(n^2) time.
    *
    * @param gridSize: Int - The length of a side of the square
    * @param grid: Array[Array[Int]] - The array for which the Summed Area Table will be calculated
    * @return: Array[Array[Int]] - The Summed Area Table
    */
  def generateSummedAreaTableNestedLoops(gridSize: Int, grid: Array[Array[Int]]): Array[Array[Int]] = {
    val summedAreaTable = Array.ofDim[Int](gridSize, gridSize)

    (1 until gridSize).foreach {
      x =>
        (1 until gridSize).foreach {
          y =>
            summedAreaTable(x)(y) = grid(x)(y) + summedAreaTable(x)(y - 1) + summedAreaTable(x - 1)(y) - summedAreaTable(x - 1)(y - 1)
        }
    }

    summedAreaTable
  }
}

trait Data {
  val gridSize = 300
  lazy val gridSerialNumber = 7511  // <-- Actual Input
}

object Timing {
  def timed[R](method: String = "method")(block: => R): R = {
    val s1 = System.nanoTime()
    val result = block
    val e1 = System.nanoTime()
    println(s"""{ "method": "$method", "timing": { ns: ${e1 - s1}, us: ${(e1 - s1) / 1000}, ms: ${(e1 - s1) / 1000 / 1000}, s: ${((e1 - s1) / 1000) / 1000 / 1000} } }""")
    result
  }
}