package org.ciroque

object AoC extends Data with App {
  println(s"Part One: ${Solution.partOne(coordinates)}")
  println(s"Part Two: ${Solution.partTwo()}")
}

object Solution {
  def partOne(coordinates: Seq[Point]): Int = {

    def buildBoundingBox(coordinates: Seq[Point]): Map[Point, Int] = {

      def minMax(values: Seq[Int]): (Int, Int) = {
        values.foldLeft((0,0)) {
          case ((min, max), value) => { (Math.min(min, value), Math.max(max, value)) }
        }
      }

      val (minX, maxX) = minMax(coordinates map { coorindate => coorindate.x })
      val (minY, maxY) = minMax(coordinates map { coorindate => coorindate.y })

//      val boundingBox: Map[Point, Int] =

      val seed: Map[Point, Int] = Map()
      val wtf = minX to maxX flatMap {
        x: Int =>
          minY to maxY map {
            y: Int =>
              Point(x, y)
          }
      }

      val x = wtf.foldLeft(seed) {
        case (map, point) => map + (point -> 0)
      }

      println(s"... ${x}")




      Map()
    }

    println(s"Testing: ${buildBoundingBox(coordinates)}")

    0
  }
  def partTwo(): Int = 0

  private def manhattanDistance(left: Point, right: Point): Int = {
    Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
  }
}

case class Point(x: Int, y: Int)

trait Data {
  lazy val coordinates = testData map(x => Point(x._1, x._2))

  lazy val testData = List(
    (1, 1),
    (1, 6),
    (8, 3),
    (3, 4),
    (5, 5),
    (8, 9)
  )

  lazy val fullData = List(

  )
}

