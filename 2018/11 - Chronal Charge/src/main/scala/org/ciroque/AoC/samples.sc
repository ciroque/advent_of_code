val GridSize: Int = 3000

def timed[R](method: String = "method")(block: => R): R = {
  val s1 = System.nanoTime()
  val result = block
  val e1 = System.nanoTime()
  println(s"""{ "method": "$method", "timing": { ns: ${e1 - s1}, us: ${(e1 - s1) / 1000}, ms: ${(e1 - s1) / 1000 / 1000}, s: ${((e1 - s1) / 1000) / 1000 / 1000} } }""")
  result
}


/* ********** ********** ********** ********** ********** ********** ********** **********
// * O(2n) implementations
**/

def generateGrid(gridSize: Int): List[Int] = {
  (for(index <- 0 until GridSize * GridSize) yield index % GridSize + 1).toList
}

def generateSummedAreaTable(gridSize: Int, grid: Array[Int]): Array[Int] = {

  def modPow2(n:Int, p2: Int): Int = n & (p2-1)
  def isPow2(n:Int):Boolean = ((n-1) & n ) == 0
  def modFast(n:Int, b: Int): Int = if (isPow2(b)) modPow2(n,b) else n % b

  // Sums the rows
  @scala.annotation.tailrec
  def recursesX(size: Int, currentIndex: Int, grid: Array[Int], tableSoFar: Array[Int]): Array[Int] = {
    if(currentIndex == size) tableSoFar
    else {
      val prevIndex = currentIndex - 1
      val nextValue = if(prevIndex < 0) 0 else tableSoFar(prevIndex)
      val multiplier = Math.min(modFast(currentIndex, gridSize), 1) // captures start of next row
//      val multiplier = Math.min(currentIndex % gridSize, 1) // captures start of next row
      tableSoFar(currentIndex) = grid(currentIndex) + (nextValue * multiplier)

      recursesX(size, currentIndex + 1, grid, tableSoFar)
    }
  }

  // sums the columns
  @scala.annotation.tailrec
  def recursesY(size: Int, currentIndex: Int, sat: Array[Int]): Array[Int] = {
    if(currentIndex == size) sat
    else {
      val currentValue = sat(currentIndex)
      val aboveIndex = currentIndex - gridSize
      val aboveValue = if(aboveIndex < 0) 0 else sat(aboveIndex)
      sat(currentIndex) = currentValue + aboveValue

      recursesY(size, currentIndex + 1, sat)
    }
  }

  val xSums = timed("recursesX") { recursesX(gridSize * gridSize, 0, grid, Array.ofDim[Int](gridSize * gridSize)) }

  timed("recursesY") { recursesY(gridSize * gridSize, 0, xSums) }
}

val grid1 = timed("generateGrid") { generateGrid(GridSize).toArray }
val sat_1 = timed("generateSummedAreaTable") { generateSummedAreaTable(GridSize, grid1) }

grid1.length
sat_1.length

/* ********** ********** ********** ********** ********** ********** ********** **********
// * O(n^2) implementations
*/
def OhNSquaredGrid(gridSize: Int): Array[Array[Int]] = {
  (0 to gridSize).map {
    x =>
      (0 to gridSize).map {
        y => (y * gridSize) + x
      }.toArray
  }.toArray
}

def OhNSquaredSAT(sat: Array[Array[Int]], grid: Array[Array[Int]], gridSize: Int): Array[Array[Int]] = {
  (1 until gridSize).foreach {
    x =>
      (1 until gridSize).foreach {
        y =>
          sat(x)(y) =  grid(x)(y) + sat(x)(y - 1) + sat(x - 1)(y) - sat(x - 1)(y - 1)
      }
  }
  sat
}

val summedAreaTable = Array.ofDim[Int](GridSize, GridSize)
val fabricatedGrid = timed("OhNSquaredGrid") { OhNSquaredGrid(GridSize) }
val r2 = timed("OhNSquaredSAT") { OhNSquaredSAT(summedAreaTable, fabricatedGrid, GridSize) }

fabricatedGrid.length
r2.length * r2.head.length
