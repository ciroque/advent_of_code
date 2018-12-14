val GridSize: Int = 300

def timed[R](method: String = "method")(block: => R): R = {
  val s1 = System.nanoTime()
  val result = block
  val e1 = System.nanoTime()
  println(s"""{ "method": "$method", "timing": { ns: ${e1 - s1}, us: ${(e1 - s1) / 1000}, ms: ${(e1 - s1) / 1000 / 1000}, s: ${((e1 - s1) / 1000) / 1000 / 1000} } }""")
  result
}

/* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****
  Grid generation
*/
def generateGridAsList(gridSize: Int): List[Int] = {
  (for (index <- 0 until GridSize * GridSize) yield index % GridSize + 1).toList
}

def generatedGrid(gridSize: Int): Array[Array[Int]] = {
  (0 to gridSize).map {
    x =>
      (0 to gridSize).map {
        y => (y * gridSize) + x
      }.toArray
  }.toArray
}


/* ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** ***** *****
  Summed Area Table algorithms
*/
def generateSummedAreaTableRecursion(gridSize: Int, grid: Array[Int]): Array[Int] = {
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

def generateSummedAreaTableTwoPasses(gridSize: Int, grid: Array[Int]): Array[Int] = {
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

def generateSummedAreaTableNestedLoops(gridSize: Int, grid: Array[Array[Int]]): Array[Array[Int]] = {
  val summedAreaTable = Array.ofDim[Int](GridSize, GridSize)

  (1 until gridSize).foreach {
    x =>
      (1 until gridSize).foreach {
        y =>
          summedAreaTable(x)(y) = grid(x)(y) + summedAreaTable(x)(y - 1) + summedAreaTable(x - 1)(y) - summedAreaTable(x - 1)(y - 1)
      }
  }

  summedAreaTable
}

val gridOne = generateGridAsList(GridSize).toArray
val gridTwo = generateGridAsList(GridSize).toArray
val gridThree = generatedGrid(GridSize)

timed("generateSummedAreaTableRecursion") { generateSummedAreaTableRecursion(GridSize, gridOne) }

timed("generateSummedAreaTableTwoPasses") { generateSummedAreaTableRecursion(GridSize, gridTwo) }

timed("generateSummedAreaTableNestedLoops") { generateSummedAreaTableNestedLoops(GridSize, gridThree) }
