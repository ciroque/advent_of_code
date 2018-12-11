import scala.collection.mutable

lazy val gridSerialNumber = 7511  // <-- Actual Input
//lazy val gridSerialNumber = 18
//lazy val gridSerialNumber = 42

lazy val GRID_SIZE = 300

// How to extract the hundreds digit
// Here is the key to immortality...
//((99 / 100) % 10) % 10

def calculatePowerLevel(x: Int, y: Int): Int = (((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) % 10) - 5

def cellsAround(grid: List[List[Int]], x: Int, y: Int): Int = {
  val one = grid(x)(y+0)
  val two = grid(x+1)(y+0)
  val three = grid(x+2)(y+0)

  val four = grid(x)(y+1)
  val five = grid(x+1)(y+1)
  val six = grid(x+2)(y+1)

  val seven = grid(x)(y+2)
  val eight = grid(x+1)(y+2)
  val nine = grid(x+2)(y+2)

  val list = List(one, two, three, four, five, six, seven, eight, nine)

//  println(s"($x, $y) -> ${list.sum} -> ${list} ")

  list.sum
}

var totalPowerLevel: Int = 0

def buildGrid(gridSize: Int, candidates: mutable.Set[(Int, Int)]): List[List[Int]] = {

  def inBounds(x: Int, y: Int): Boolean = (x + 2) <= gridSize && (y + 2) <= gridSize

  (0 to gridSize).map {
    x: Int =>
      (0 to gridSize).map {
        y: Int =>
          val p = calculatePowerLevel(x, y)
          if(p == 4 && inBounds(x, y))  {
            candidates.add((x, y))
          }
          totalPowerLevel = totalPowerLevel + p
          p
      }.toList
  }.toList
}

def findLargestTotalPower(gridSize: Int, candidates: List[(Int, Int)], grid: List[List[Int]], highestPowerLevel: Int, currentHighestPoint: (Int, Int)): ((Int, Int), Int)  = {
  candidates match {
    case Nil => (currentHighestPoint, highestPowerLevel)
    case (x, y) :: tail => {
      val sum = cellsAround(grid, x, y)

      val nextHighestPowerLevel = if(sum > highestPowerLevel) sum else highestPowerLevel
      val nextHighestPoint = if(sum > highestPowerLevel) (x,y) else currentHighestPoint

      findLargestTotalPower(gridSize, tail, grid, nextHighestPowerLevel, nextHighestPoint)
    }
  }
}

var candidateCells = mutable.Set[(Int, Int)]()
val grid = buildGrid(GRID_SIZE, candidateCells)

findLargestTotalPower(3, candidateCells.toList, grid, 0, (-1, -1))

println(s"UNLIMITED POWER: $totalPowerLevel")
