case class Point(x: Int, y: Int)

def manhattanDistance(left: Point, right: Point): Int = {
  Math.abs(left.x - right.x) + Math.abs(left.y - right.y)
}

manhattanDistance(Point(1,1), Point(6,7))
