import scala.collection.mutable

/* ---------------------------------------------------------------------------------------------------------------------
  data structures
 */

case class Point(x: Int, y: Int, tag: Char) {
  override def toString: String = tag.toString
}

/* ---------------------------------------------------------------------------------------------------------------------
  helpers
 */

def minMax(values: Seq[Int]): (Int, Int) = {
  values.foldLeft((Int.MaxValue, Int.MinValue)) {
    case ((min, max), value) => (Math.min(min, value), Math.max(max, value))
  }
}

def manhattanDistance(left: Point, right: Point): Int = {
  Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
}

/* ---------------------------------------------------------------------------------------------------------------------
  values
*/

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

lazy val chars = ('A' to 'Z').toList ::: ('a' to 'z').toList

//lazy val coordinates: List[Point] = testData.zipWithIndex.map { case((x, y), index) => Point(x, y, chars(index)) } sortBy { p => (p.x, p.y) }
lazy val coordinates: List[Point] = fullData.zipWithIndex.map { case((x, y), index) => Point(x, y, chars(index)) } sortBy { p => (p.x, p.y) }

lazy val inputLength = coordinates.length

lazy val (minX, maxX) = minMax(coordinates map { coordinate => coordinate.x })
lazy val (minY, maxY) = minMax(coordinates map { coordinate => coordinate.y })

var owned = mutable.MutableList.fill(inputLength)(0)
var infinite: mutable.MutableList[Boolean] = mutable.MutableList.fill(inputLength)(false)
var within = 0

val MAX_DISTANCE = 10000

/* ---------------------------------------------------------------------------------------------------------------------
  Part One
 */

def partOne() = {
  val yRange = minY to maxY
  val xRange = minX to maxX

  yRange.foreach {
    y =>

      val yEdge = y == yRange.head || y == yRange.last
      val yDistances = coordinates.map(coordinate => Math.abs(coordinate.y - y))

      xRange.foreach {
        x =>

          var bestDistance = 1.0 / 0.0
          var best = -1
          var totalDistance = 0

          coordinates.zipWithIndex.foreach {
            case(coordinate: Point, index: Int) =>
              val distance = yDistances(index) + Math.abs(coordinate.x - x)
              if(distance < bestDistance) {
                best = index
                bestDistance = distance
              } else if(distance == bestDistance) {
                best = -1
              }
              totalDistance += distance
          }

          val xEdge = x == xRange.head || x == xRange.last

          if(totalDistance < MAX_DISTANCE) {
            within += 1
//            if(yEdge || xEdge) {
//              println(s"DANGER: EDGE DETECTED: ($x, $y)")
//            }
          }

          // next unless best -- how to do in Scala?

          if(yEdge || xEdge) {
            infinite(best) = true
          } else {
            owned(best) = owned(best) + 1
          }
      }
  }

  val answer = owned.zip(infinite).filter(_._2 == false).minBy(-_._1)
  (answer._1, within)
//  owned.zip(infinite).sortBy(-_._1)
}

println(s"Part One: ${partOne()}")
