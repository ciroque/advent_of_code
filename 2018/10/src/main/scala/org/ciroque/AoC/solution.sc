case class PointOfLight(group: Int, x: Int, y: Int, deltaX: Int, deltaY: Int)

lazy val PositionExtractor = """position=\<(.*),(.*)\> velocity=\<(.*),(.*)\>""".r

lazy val testData = List(
  "position=< 9,  1> velocity=< 0,  2>",
  "position=< 7,  0> velocity=<-1,  0>",
  "position=< 3, -2> velocity=<-1,  1>",
  "position=< 6, 10> velocity=<-2, -1>",
  "position=< 2, -4> velocity=< 2,  2>",
  "position=<-6, 10> velocity=< 2, -2>",
  "position=< 1,  8> velocity=< 1, -1>",
  "position=< 1,  7> velocity=< 1,  0>",
  "position=<-3, 11> velocity=< 1, -2>",
  "position=< 7,  6> velocity=<-1, -1>",
  "position=<-2,  3> velocity=< 1,  0>",
  "position=<-4,  3> velocity=< 2,  0>",
  "position=<10, -3> velocity=<-1,  1>",
  "position=< 5, 11> velocity=< 1, -2>",
  "position=< 4,  7> velocity=< 0, -1>",
  "position=< 8, -2> velocity=< 0,  1>",
  "position=<15,  0> velocity=<-2,  0>",
  "position=< 1,  6> velocity=< 1,  0>",
  "position=< 8,  9> velocity=< 0, -1>",
  "position=< 3,  3> velocity=<-1,  1>",
  "position=< 0,  5> velocity=< 0, -1>",
  "position=<-2,  2> velocity=< 2,  0>",
  "position=< 5, -2> velocity=< 1,  2>",
  "position=< 1,  4> velocity=< 2,  1>",
  "position=<-2,  7> velocity=< 2, -2>",
  "position=< 3,  6> velocity=<-1, -1>",
  "position=< 5,  0> velocity=< 1,  0>",
  "position=<-6,  0> velocity=< 2,  0>",
  "position=< 5,  9> velocity=< 1, -2>",
  "position=<14,  7> velocity=<-2,  0>",
  "position=<-3,  6> velocity=< 2, -1>"
)

lazy val positionData: List[PointOfLight] = testData.map {
  case PositionExtractor(x, y, deltaX, deltaY) =>
//    println(s"x: ${x.trim.toInt}, y: ${y.trim.toInt}, deltaX: ${deltaX.trim.toInt}, deltaY: ${deltaY.trim.toInt}")
    PointOfLight(0, x.trim.toInt, y.trim.toInt, deltaX.trim.toInt, deltaY.trim.toInt)
}

def partOne() = {
  (0 to 10).map {
    index: Int => {
      val PointOfLight(_, x, y, deltaX, deltaY) = positionData(index)
      PointOfLight(index, x + (index * deltaX), y + (index * deltaY), deltaX, deltaY)
    }
  }
}

positionData
