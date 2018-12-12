package org.ciroque

case class Coordinate(x: Int, y: Int)

case class FuelCell(index: Int, coordinate: Coordinate, powerLevel: Int)


object AoC extends Data with App {
  println(s"Part One: ${Solution.partOne(gridSerialNumber, GridSize)}")
  println(s"Part Two: ${Solution.partTwo(gridSerialNumber, GridSize)}")
}

object Solution {
  lazy val SeedFuelCell: FuelCell = FuelCell(-1, Coordinate(-1, -1), -1)

  private def createLookAroundOffsets(distance: Int, gridSize: Int): List[(Int, Int)] = {
    (for(
      i <- 0 until gridSize * gridSize;
      x = i % gridSize if x < distance;
      y = i / gridSize if y < distance
    )
      yield(x, y)).toList
  }

  private def createPowerMatrix(gridSerialNumber: Int, gridSize: Int): List[FuelCell] = {
    def calculatePowerLevel(x: Int, y: Int): Int = (((((((x + 10) * y) + gridSerialNumber) * (x + 10)) / 100) % 10) % 10) - 5

    (for(
      i <- 0 until (gridSize * gridSize);
      x = i % gridSize;
      y = i / gridSize
    )
      yield FuelCell(i, Coordinate(x, y), calculatePowerLevel(x, y))).toList
  }

  private def findCandidates(gridSize: Int, targetGridSize: Int, powerMatrix: List[FuelCell]): List[FuelCell] = {
    powerMatrix.filter {
      pc =>
//        pc.powerLevel == 4 &&
          (pc.coordinate.x + targetGridSize) <= gridSize &&
          (pc.coordinate.y + targetGridSize) <= gridSize
    }
  }

  private def findHighestPowerLevel(lookArounds: List[(Int, Int)], candidates: List[FuelCell], power: Map[(Int, Int), Int], currentHighest: FuelCell): FuelCell = {
    candidates match {
      case Nil => currentHighest
      case head :: tail =>
        val sum = lookArounds.map { la => power((head.coordinate.x + la._1, head.coordinate.y + la._2))}.sum
//        println(s">>> ${lookArounds.length} SUM: $sum")
        val nextCurrentHighest = if(sum > currentHighest.powerLevel) FuelCell(head.index, head.coordinate, sum) else currentHighest
        findHighestPowerLevel(lookArounds, tail, power, nextCurrentHighest)
    }
  }

  def partOne(gridSerialNumber: Int, gridSize: Int): FuelCell = {
    val TargetGridSize = 3
    val powerMatrix = createPowerMatrix(gridSerialNumber, gridSize)
    val candidateCells = findCandidates(gridSize, TargetGridSize, powerMatrix)
    val lookAroundOffsets = createLookAroundOffsets(TargetGridSize, gridSize)
    val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap
    findHighestPowerLevel(lookAroundOffsets, candidateCells, power, SeedFuelCell)
  }

  def partTwo(gridSerialNumber: Int, gridSize: Int): FuelCell = {
    val powerMatrix = createPowerMatrix(gridSerialNumber, gridSize)
    val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap

    def recurses(targetGridSize: Int, currentHighest: FuelCell, lastSum: Int): FuelCell = {
      val lookAroundOffsets = createLookAroundOffsets(targetGridSize, gridSize)
      val candidateCells = findCandidates(gridSize, targetGridSize, powerMatrix)

      if(lastSum < 0) currentHighest
      else {
        targetGridSize match {
          case 0 => currentHighest
          case _ => {
            val highestInTargetGridSize = findHighestPowerLevel(lookAroundOffsets, candidateCells, power, SeedFuelCell)

            val nextHighest = if(highestInTargetGridSize.powerLevel > currentHighest.powerLevel) {
              FuelCell(
                targetGridSize,
                Coordinate(highestInTargetGridSize.coordinate.x - 1, highestInTargetGridSize.coordinate.y + 1),
                highestInTargetGridSize.powerLevel
              )
            } else {
              currentHighest

            }

            recurses(targetGridSize + 1, nextHighest, highestInTargetGridSize.powerLevel)
          }
        }
      }
    }

    recurses(1, SeedFuelCell, 0)
  }
}

trait Data {
  val GridSize = 300
//  lazy val gridSerialNumber = 7511  // <-- Actual Input
  lazy val gridSerialNumber = 18
//  lazy val gridSerialNumber = 42
}

