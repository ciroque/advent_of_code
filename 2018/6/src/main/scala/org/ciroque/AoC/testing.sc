import scala.annotation.tailrec

val chars = ('A' to 'Z').toList ::: ('a' to 'z').toList

case class Point(x: Int, y: Int, tag: Char) {
  override def toString: String = tag.toString
}

/* *********************************************************************************************************************
  Functions:
*/
def manhattanDistance(left: Point, right: Point): Int = {
  Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
}

def minMax(values: Seq[Int]): (Int, Int) = {
  values.foldLeft((Int.MaxValue, Int.MinValue)) {
    case ((min, max), value) => (Math.min(min, value), Math.max(max, value))
  }
}

def closestTo(coord: Point, points: List[Point]): List[Point] = {
//  println(s"point: $coord")
  val distances = points map {
    point: Point =>
      point -> manhattanDistance(point, coord)
  }

  val sorted = distances.sortBy(_._2)
//  println(s"sorted: ${sorted}")

  val shortest = sorted.head._2
//  println(s"shortest: $shortest")

  val filtered = sorted.filter(_._2 == shortest)
//  println(s"filtered: $filtered")

  val mapped = filtered map(_._1)
//  println(s"mapped: $mapped")

//  println("--------- --------- --------- --------- --------- --------- ---------")

  mapped
}

/* *********************************************************************************************************************
  Values:
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

lazy val coordinates: List[Point] = fullData.zipWithIndex.map { case((x, y), index) => Point(x, y, chars(index)) } sortBy { p => (p.x, p.y) } //(case(()) => Point(x._1, x._2))
//lazy val coordinates: List[Point] = testData.zipWithIndex.map { case((x, y), index) => Point(x, y, chars(index)) } sortBy { p => (p.x, p.y) } //(case(()) => Point(x._1, x._2))

lazy val (minX, maxX) = minMax(coordinates map { coordinate => coordinate.x })
lazy val (minY, maxY) = minMax(coordinates map { coordinate => coordinate.y })
lazy val width = maxX - minX
lazy val height = maxY - minY
lazy val boundingArea = width * height

lazy val boundingBoxPoints = minX to maxX flatMap {
  x: Int =>
    minY to maxY map {
      y: Int =>
        Point(x, y, '0')
    }
}

coordinates foreach { c => println(s"${c.tag} : ${c.x} : ${c.y}") }

val center = coordinates.length / 2
val even = coordinates.length % 2 == 0
println(s"CANDIDATE: ${coordinates(center)}")
if(even) println(s"CANDIDATE: ${coordinates(center-1)}")


/* *********************************************************************************************************************
  Implementation:
*/

@scala.annotation.tailrec
def recurses(map: List[(Point, List[Point])], points: List[Point]): List[(Point, Seq[Point])] = {
  points match {
    case Nil => map
    case head :: tail =>
      val next = (head, closestTo(head, coordinates))
      recurses(next +: map, tail)
  }
}

def partOne(): Unit = {
  println(s"Coordinate Count: ${coordinates.length} minX: $minX, maxX: $maxX, minY: $minY, maxY: $maxY, width: ${maxX - minX}, height: ${maxY - minY}")

  // calculates the list of Coordinates that are closest to the bounding box Points
  // The list has one element if there is only one closest Coordinate,
  // or more elements if there is a tie.
  val closestPoints = recurses(List(), boundingBoxPoints.toList)
//  closestPoints foreach { cp => println(s"$cp")}

  // filters out Points that are tied for distance from the bounding box Point
  val filtered = closestPoints.filter { x => x._2.length == 1 }
//  println(s"Filtered: $filtered")

  // We want just the first Coordinate as that is the closest
  val mapped = filtered.map(_._2.head)

  // here we roll up the closest Coordinates and sort them by Point count (the area)
  val grouped = mapped.groupBy(identity ).mapValues(_.size).toList.sortBy(-_._2)

  // filter any Coordinates that are along the boundary
  val finalList = grouped.filter { case (point, _) => point.x > minX && point.x < maxX && point.y > minY && point.y < maxY }

  // Print them out...
//  grouped foreach(point => println(s"group: $point"))
  println("----- SOLUTION -----")
  finalList foreach(point => println(s"group: $point"))
  println("----- SOLUTION -----")


  // ACHTUNG!
  // The biggest problem is that the algorithm does not detect Coordinates that are infinite.
  // That is a big problem...
}

def drawGrid(): Unit = {
  val closestPoints = recurses(List(), boundingBoxPoints.toList).toMap
  val boundingBox = boundingBoxPoints.toList

  def getMark(closest: List[Point]): String = {
    if(closest.length == 1)
      closest.head.toString
    else
      "."
  }

  boundingBox.zipWithIndex.foreach {
    case(coord, index) =>
      print(s"${getMark(closestPoints(coord).toList)} ")
      if(index % width == 0)
        println("")
  }
}

partOne()

//drawGrid()


