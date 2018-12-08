package org.ciroque

object AoC extends Data with App {
  println(s"Part One: ${Solution.partOne(coordinates)}")
  println(s"Part Two: ${Solution.partTwo()}")
}

object Solution {

  def partOne(coordinates: Seq[Point]): Int = {

    // positive we need these to let us know if we are head into the weeds (infinity...)
    lazy val (minX, maxX) = minMax(coordinates map { coordinate => coordinate.x })
    lazy val (minY, maxY) = minMax(coordinates map { coordinate => coordinate.y })

    // Not convinced this is necessary, but positive the top-left and bottom-right points are needed
    def buildBoundingBox(coordinates: Seq[Point]): Map[Point, Int] = {
      val seed: Map[Point, Int] = Map()
      val boundingBoxPoints = minX to maxX flatMap {
        x: Int =>
          minY to maxY map {
            y: Int =>
              Point(x, y)
          }
      }

      boundingBoxPoints.foldLeft(seed) {
        case (map, point) => map + (point -> 0)
      }
    }

    def distanceFromEdge(coords: Seq[Point]): Seq[(Point, (Int, Int, Int, Int))] = {
      coords map {
        coord =>
          (coord -> (coord.x - minX, maxX - coord.x, coord.y - minY, maxY - coord.y))
      }
    }


    //    println(s"Testing: ${buildBoundingBox(coordinates)}")

    println(s"MinX: $minX, MaxX: $maxX, MinY: $minY, MaxY: $maxY")


    println(s">>>> ${distanceFromEdge(coordinates).sortBy(x => x._2._2 + x._2._3).reverse}")

    0
  }

  def partTwo(): Int = 0

  private def calculateManhattanDistance(left: Point, right: Point): Int = {
    Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
  }

  private def minMax(values: Seq[Int]): (Int, Int) = {
    values.foldLeft((Int.MaxValue, Int.MinValue)) {
      case ((min, max), value) => { (Math.min(min, value), Math.max(max, value)) }
    }
  }

  private def manhattanDistance(left: Point, right: Point): Int = {
    Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
  }
}

case class Point(x: Int, y: Int)

trait Data {
//  lazy val coordinates = testData map(x => Point(x._1, x._2))
  lazy val coordinates = fullData map(x => Point(x._1, x._2))

  lazy val testData = List(
    (1, 1),
    (1, 6),
    (8, 3),
    (3, 4),
    (5, 5),
    (8, 9)
  )

  lazy val fullData = List(
    (181, 184),
    (230, 153),
    (215, 179),
    (84, 274),
    (294, 274),
    (127, 259),
    (207, 296),
    (76, 54),
    (187, 53),
    (318, 307),
    (213, 101),
    (111, 71),
    (310, 295),
    (40, 140),
    (176, 265),
    (98, 261),
    (315, 234),
    (106, 57),
    (40, 188),
    (132, 292),
    (132, 312),
    (97, 334),
    (292, 293),
    (124, 65),
    (224, 322),
    (257, 162),
    (266, 261),
    (116, 122),
    (80, 319),
    (271, 326),
    (278, 231),
    (191, 115),
    (277, 184),
    (329, 351),
    (58, 155),
    (193, 147),
    (45, 68),
    (310, 237),
    (171, 132),
    (234, 152),
    (158, 189),
    (212, 100),
    (346, 225),
    (257, 159),
    (330, 112),
    (204, 320),
    (199, 348),
    (207, 189),
    (130, 289),
    (264, 223)
  )
}

