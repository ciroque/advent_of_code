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

    val coreList = (for(
      i <- 0 until (gridSize * gridSize);
      x = i % gridSize;
      y = i / gridSize
    )
    yield FuelCell(i, Coordinate(x, y), calculatePowerLevel(x, y))).toList

    val baseIndex = gridSize * gridSize

    val xMaxList = (0 until gridSize).map(index => FuelCell(baseIndex + index, Coordinate(index, 300), calculatePowerLevel(index, 300)))
    val yMaxList = (0 until gridSize).map(index => FuelCell(baseIndex + index + gridSize, Coordinate(300, index), calculatePowerLevel(300, index)))

    coreList ++ xMaxList ++ yMaxList
  }

  // https://en.wikipedia.org/wiki/Summed-area_table
  // I(x,y) = i(x,y) + I(x, y - 1) + I(x - 1, y) + I(x - 1, y - 1)
  private def createSummedAreaTable(gridSize: Int, powerMatrix: List[FuelCell]): List[Int] = {
    val startIndex = (1 * gridSize) + 1
    val length = gridSize * gridSize
    val fuelCells = powerMatrix.toArray
    ((startIndex to gridSize * gridSize).map {
      index => fuelCells(index).powerLevel + fuelCells(index - gridSize).powerLevel + fuelCells(index - 1).powerLevel + fuelCells(index - gridSize - 1).powerLevel
    }).toList
  }


  private def findCandidates(gridSize: Int, targetGridSize: Int, powerMatrix: List[FuelCell]): List[FuelCell] = {
    powerMatrix.filter {
      pc =>
        pc.powerLevel > 0 &&
          (pc.coordinate.x + targetGridSize) <= gridSize &&
          (pc.coordinate.y + targetGridSize) <= gridSize
    }
  }

  private def findHighestPowerLevel(lookArounds: List[(Int, Int)], candidates: List[FuelCell], power: Map[(Int, Int), Int], currentHighest: FuelCell): FuelCell = {
    candidates match {
      case Nil => currentHighest
      case head :: tail =>
        val sum = lookArounds.map { la => power((head.coordinate.x + la._1, head.coordinate.y + la._2))}.sum
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
    val s1 = System.currentTimeMillis()
    val powerMatrix = createPowerMatrix(gridSerialNumber, gridSize)
    val e1 = System.currentTimeMillis()

    val s2 = System.currentTimeMillis()
    createSummedAreaTable(gridSize, powerMatrix)
    val e2 = System.currentTimeMillis()

    println(
      s"""
         | createPowerMatrix took ${(e1 - s1) / 1000} seconds
         | createSummedAreaTable took ${(e2 - s2) / 1000} seconds
         |
       """.stripMargin)

//    powerMatrix.foreach(fc => println(fc.index))

//    val power = powerMatrix.map { cell => (cell.coordinate.x, cell.coordinate.y) -> cell.powerLevel }.toMap
//
//    def recurses(targetGridSize: Int, currentHighest: FuelCell, lastSum: Int): FuelCell = {
//      val lookAroundOffsets = createLookAroundOffsets(targetGridSize, gridSize)
//      val candidateCells = findCandidates(gridSize, targetGridSize, powerMatrix)
//
//      targetGridSize match {
//        case 0 => currentHighest
//        case _ => {
//          val highestInTargetGridSize = findHighestPowerLevel(lookAroundOffsets, candidateCells, power, SeedFuelCell)
//
//          val nextHighest = if(highestInTargetGridSize.powerLevel > currentHighest.powerLevel) {
//            FuelCell(
//              targetGridSize,
//              Coordinate(highestInTargetGridSize.coordinate.x - 1, highestInTargetGridSize.coordinate.y + 1),
//              highestInTargetGridSize.powerLevel
//            )
//          } else {
//            currentHighest
//
//          }
//
//          recurses(targetGridSize + 1, nextHighest, highestInTargetGridSize.powerLevel)
//        }
//      }
//    }
//
//    recurses(1, SeedFuelCell, 0)

    SeedFuelCell
  }
}

trait Data {
  val GridSize = 300
  lazy val gridSerialNumber = 7511  // <-- Actual Input
//  lazy val gridSerialNumber = 18
//  lazy val gridSerialNumber = 42
}

