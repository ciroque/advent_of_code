/*

  Represent the grid as an array

 */

300 % 300

(23 * 300) + 21




val GridSize = 300

lazy val gridSerialNumber = 7511  // <-- Actual Input
//lazy val gridSerialNumber = 18
//lazy val gridSerialNumber = 42

case class Coordinate(x: Int, y: Int)

case class FuelCell(index: Int, coordinate: Coordinate, powerLevel: Int)

lazy val SeedFuelCell: FuelCell = FuelCell(-1, Coordinate(-1, -1), -1)

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

def findHighestPowerLevel(lookArounds: List[(Int, Int)], candidates: List[FuelCell], power: Map[(Int, Int), Int], currentHighest: FuelCell): FuelCell = {
  candidates match {
    case Nil => currentHighest
    case head :: tail =>
      val sum = lookArounds.map { la => power((head.coordinate.x + la._1, head.coordinate.y + la._2))}.sum
      val nextCurrentHighest = if(sum > currentHighest.powerLevel) FuelCell(head.index, head.coordinate, sum) else currentHighest
      findHighestPowerLevel(lookArounds, tail, power, nextCurrentHighest)
  }
}

def partOne(): FuelCell = {
  val TargetGridSize = 3
  val powerMatrix = createPowerMatrix(GridSize)
  val candidateCells = findCandidates(GridSize, TargetGridSize, powerMatrix)
  val lookAroundOffsets = createLookAroundOffsets(TargetGridSize, GridSize)
  val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap
  findHighestPowerLevel(lookAroundOffsets, candidateCells, power, SeedFuelCell)
}


def partTwo(): FuelCell = {
  val powerMatrix = createPowerMatrix(GridSize)
  val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap

  def recurses(targetGridSize: Int, currentHighest: FuelCell): FuelCell = {
    println(s"... $currentHighest ... $targetGridSize")
    val candidateCells = findCandidates(GridSize, targetGridSize, powerMatrix)
    val lookAroundOffsets = createLookAroundOffsets(targetGridSize, GridSize)

    targetGridSize match {
      case 0 => currentHighest
      case _ => {
        val highestInTargetGridSize = findHighestPowerLevel(lookAroundOffsets, candidateCells, power, SeedFuelCell)
        val nextHighest = if(highestInTargetGridSize.powerLevel > currentHighest.powerLevel) highestInTargetGridSize else currentHighest
        recurses(targetGridSize - 1, currentHighest)
      }
    }
  }

  recurses(GridSize, SeedFuelCell)
}

println(s"Part One: ${partOne()}")
println(s"Part Two: ${partTwo()}")
