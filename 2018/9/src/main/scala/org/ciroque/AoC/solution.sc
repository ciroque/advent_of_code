case class Result(players: Int, lastMarblePoints: Int, highScore: Int)

lazy val testData: List[Result] = List(
//  Result(9, 25, 32),
//  Result(10, 1618, 8317),
//  Result(13, 7999, 146373),
//  Result(17, 1104, 2764),
//  Result(21, 6111, 54718),
//  Result(30, 5807, 37305),
//  Result(470, 72170, 0) // <-- Part One
  Result(470, 7217000, 0) // <-- Part Two
)

lazy val fullData: List[Result] = List(Result(470, 72170, 0))

def multipleOf23(n: Int): Boolean = n % 23 == 0

def partOne(nPlayers: Int, nMarbles: Int): Int = {
  @scala.annotation.tailrec
  def recurses(board: List[Int], currentMarble: Int, previousIndex: Int, player: Int, playerPoints: Map[Int, Int]): Int = {
    val nextMarble = currentMarble + 1

    if(currentMarble == nMarbles) {
      val points = playerPoints.toList.maxBy(_._2)._2
      points
    } else {

      if(multipleOf23(currentMarble)) {
        var indexToRemove = previousIndex - 7
        indexToRemove = if(indexToRemove < 0) board.length + indexToRemove else indexToRemove

//        println(s"LENGTH: ${board.length}, TO REMOVE: ${indexToRemove}, POINTS: ${playerPoints}")

        // retrieve the value to be removed for the points...
        val points = board(indexToRemove) + currentMarble
        val previousPoints = playerPoints.getOrElse(player, 0)
        val newPoints = previousPoints + points

        // remove element 7 to the left...
        val nextBoard = board.take(indexToRemove) ++ board.drop(indexToRemove + 1)

//        println(s"[$player]($points) $nextBoard")

        recurses(nextBoard, nextMarble, indexToRemove, (player % nPlayers) + 1, playerPoints + (player -> newPoints))
      } else {
        val nextIndex = if(previousIndex == 0) 1 else {
          ((previousIndex + 1) % board.length) + 1
        }

        val (front, back) = board.splitAt(nextIndex)
        val nextBoard = front ++ List(currentMarble) ++ back

//        println(s"[$player] $nextBoard")

        recurses(nextBoard, nextMarble, nextIndex, (player % nPlayers) + 1, playerPoints)
      }
    }
  }

  recurses(List(0), 1, 0, 1, Map[Int, Int]())
}

testData.foreach { result =>
  println(s"Part One: ${result.players} players, ${result.lastMarblePoints} marbles, High Score (expected): ${result.highScore})")
  val actual = partOne(result.players, result.lastMarblePoints)
  println(s"Actual Score: ${actual} DIFFERENCE: ${result.highScore - actual}")
  println("----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- ----- -----")
}

/*
3180929875
*/

