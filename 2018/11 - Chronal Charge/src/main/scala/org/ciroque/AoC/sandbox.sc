/*

  Represent the grid as an array

 */

lazy val gridSerialNumber = 7511  // <-- Actual Input
//lazy val gridSerialNumber = 18
//lazy val gridSerialNumber = 42

case class Coordinate(x: Int, y: Int)
case class FuelCell(index: Int, coordinate: Coordinate, powerLevel: Int)

val GridSize = 300

def createLookAroundOffsets(distance: Int, gridSize: Int): List[(Int, Int)] = {
  (for(
    i <- 0 until gridSize * gridSize;
    x = i % gridSize if x < distance;
    y = i / gridSize if y < distance
  )
    yield(x, y)).toList
}

def createPowerMatrix(size: Int): List[FuelCell] = {
  def calculatePowerLevel(x: Int, y: Int): Int = (((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) % 10) - 5

  (for(
    i <- 0 until (GridSize * GridSize);
    x = i % GridSize;
    y = i / GridSize
  )
  yield FuelCell(i, Coordinate(x, y), calculatePowerLevel(x, y))).toList
}

def findCandidates(gridSize: Int, targetGridSize: Int, powerMatrix: List[FuelCell]): List[FuelCell] = {
  powerMatrix.filter {
    pc =>
      pc.powerLevel == 4 &&
      (pc.coordinate.x + targetGridSize) <= gridSize &&
      (pc.coordinate.y + targetGridSize) <= gridSize
  }
}

def partOne(): FuelCell = {
  val TargetGridSize = 3
  val powerMatrix = createPowerMatrix(GridSize)
  val candidateCells = findCandidates(GridSize, TargetGridSize, powerMatrix)
  val lookAroundOffsets = createLookAroundOffsets(TargetGridSize, GridSize)

  def recurses(lookArounds: List[(Int, Int)], candidates: List[FuelCell], power: Map[(Int, Int), Int], currentHighest: FuelCell): FuelCell = {
    candidates match {
      case Nil => currentHighest
      case head :: tail =>
        val sum = lookArounds.map { la => power((head.coordinate.x + la._1, head.coordinate.y + la._2))}.sum
        val nextCurrentHighest = if(sum > currentHighest.powerLevel) FuelCell(head.index, head.coordinate, sum) else currentHighest
        recurses(lookArounds, tail, power, nextCurrentHighest)
    }
  }

  val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap
  recurses(lookAroundOffsets, candidateCells, power, FuelCell(-1, Coordinate(-1, -1), -1))
}


def partTwo(): (Int, Int, Int) = {

  (-1, -1, -1)
}

println(s"Part One: ${partOne()}")
