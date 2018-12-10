import scala.collection.immutable.Queue

case class Step(label: Char, dependent: Char)

lazy val testData = List(
  "Step C must be finished before step A can begin.",
  "Step C must be finished before step F can begin.",
  "Step A must be finished before step B can begin.",
  "Step A must be finished before step D can begin.",
  "Step B must be finished before step E can begin.",
  "Step D must be finished before step E can begin.",
  "Step F must be finished before step E can begin."
)

lazy val fullData = List(
  "Step F must be finished before step N can begin.",
  "Step V must be finished before step Y can begin.",
  "Step B must be finished before step W can begin.",
  "Step K must be finished before step O can begin.",
  "Step E must be finished before step H can begin.",
  "Step A must be finished before step P can begin.",
  "Step Y must be finished before step S can begin.",
  "Step T must be finished before step L can begin.",
  "Step G must be finished before step R can begin.",
  "Step I must be finished before step H can begin.",
  "Step X must be finished before step M can begin.",
  "Step N must be finished before step C can begin.",
  "Step O must be finished before step R can begin.",
  "Step Z must be finished before step R can begin.",
  "Step R must be finished before step D can begin.",
  "Step M must be finished before step C can begin.",
  "Step H must be finished before step D can begin.",
  "Step C must be finished before step U can begin.",
  "Step J must be finished before step D can begin.",
  "Step L must be finished before step Q can begin.",
  "Step D must be finished before step U can begin.",
  "Step S must be finished before step U can begin.",
  "Step U must be finished before step Q can begin.",
  "Step P must be finished before step Q can begin.",
  "Step W must be finished before step Q can begin.",
  "Step X must be finished before step R can begin.",
  "Step P must be finished before step W can begin.",
  "Step B must be finished before step U can begin.",
  "Step E must be finished before step J can begin.",
  "Step T must be finished before step R can begin.",
  "Step M must be finished before step L can begin.",
  "Step M must be finished before step P can begin.",
  "Step V must be finished before step T can begin.",
  "Step T must be finished before step U can begin.",
  "Step R must be finished before step W can begin.",
  "Step V must be finished before step A can begin.",
  "Step X must be finished before step S can begin.",
  "Step V must be finished before step U can begin.",
  "Step C must be finished before step P can begin.",
  "Step J must be finished before step S can begin.",
  "Step F must be finished before step D can begin.",
  "Step Y must be finished before step U can begin.",
  "Step L must be finished before step W can begin.",
  "Step F must be finished before step T can begin.",
  "Step B must be finished before step E can begin.",
  "Step F must be finished before step J can begin.",
  "Step R must be finished before step M can begin.",
  "Step Z must be finished before step W can begin.",
  "Step K must be finished before step E can begin.",
  "Step S must be finished before step W can begin.",
  "Step U must be finished before step P can begin.",
  "Step S must be finished before step P can begin.",
  "Step D must be finished before step W can begin.",
  "Step Z must be finished before step P can begin.",
  "Step U must be finished before step W can begin.",
  "Step M must be finished before step J can begin.",
  "Step M must be finished before step W can begin.",
  "Step H must be finished before step U can begin.",
  "Step E must be finished before step C can begin.",
  "Step C must be finished before step Q can begin.",
  "Step L must be finished before step U can begin.",
  "Step Y must be finished before step R can begin.",
  "Step E must be finished before step D can begin.",
  "Step A must be finished before step S can begin.",
  "Step Z must be finished before step J can begin.",
  "Step X must be finished before step W can begin.",
  "Step C must be finished before step D can begin.",
  "Step C must be finished before step S can begin.",
  "Step G must be finished before step N can begin.",
  "Step K must be finished before step Z can begin.",
  "Step T must be finished before step I can begin.",
  "Step H must be finished before step W can begin.",
  "Step E must be finished before step Q can begin.",
  "Step R must be finished before step J can begin.",
  "Step O must be finished before step H can begin.",
  "Step O must be finished before step J can begin.",
  "Step L must be finished before step S can begin.",
  "Step A must be finished before step H can begin.",
  "Step K must be finished before step G can begin.",
  "Step I must be finished before step X can begin.",
  "Step T must be finished before step W can begin.",
  "Step O must be finished before step W can begin.",
  "Step N must be finished before step Q can begin.",
  "Step V must be finished before step Z can begin.",
  "Step H must be finished before step S can begin.",
  "Step F must be finished before step L can begin.",
  "Step X must be finished before step Z can begin.",
  "Step I must be finished before step U can begin.",
  "Step T must be finished before step J can begin.",
  "Step G must be finished before step S can begin.",
  "Step E must be finished before step U can begin.",
  "Step M must be finished before step U can begin.",
  "Step J must be finished before step U can begin.",
  "Step E must be finished before step P can begin.",
  "Step F must be finished before step C can begin.",
  "Step O must be finished before step Q can begin.",
  "Step D must be finished before step Q can begin.",
  "Step A must be finished before step L can begin.",
  "Step H must be finished before step J can begin.",
  "Step I must be finished before step P can begin.",
  "Step Y must be finished before step D can begin."
)

val stepExtractor = """Step ([A-Za-z@]) must be finished before step ([A-Za-z]) can begin.""".r

//lazy val steps: List[Step] = testData.map {
lazy val steps: List[Step] = fullData.map {
  case stepExtractor(label, dependent) => Step(label.head, dependent.head)
}.sortBy(_.label)

type DependenciesMap = Map[Char, List[Char]]
type DependenceMaps = (DependenciesMap, DependenciesMap)

// ---------------------------------------------------------------------------------------------------------------------
// Need to build Trees of both dependencies and requisites in order to determine the proper order
// Using a List of Lists implementation to represent the tree
@scala.annotation.tailrec
def buildDependencyMap(dependenceMaps: DependenceMaps, steps: List[Step]): DependenceMaps = {
  steps match {
    case Nil => dependenceMaps
    case head :: tail =>

      val existing1: List[Char] = dependenceMaps._1.getOrElse(head.label, List[Char]())
      val updated1: List[Char] = (head.dependent +: existing1).sorted
      val dependencies = dependenceMaps._1 + (head.label -> updated1)

      val existing2: List[Char] = dependenceMaps._2.getOrElse(head.dependent, List[Char]())
      val updated2: List[Char] = (head.label +: existing2).sorted
      val requisites = dependenceMaps._2 + (head.dependent -> updated2)

      buildDependencyMap((dependencies, requisites), tail)
  }
}

// ---------------------------------------------------------------------------------------------------------------------
// Need to know the Node(s) from which to start Graph traversal
def findStartingSteps(): List[Char] = {
  val requisites = steps.map(_.label).distinct
  val dependencies = steps.map(_.dependent).distinct
  (requisites diff dependencies).sorted
}

// ---------------------------------------------------------------------------------------------------------------------
// Breadth-First Search of dependencies tree with requisite testing determines the final answer
def buildList(): String = {
  val dependencyTree = buildDependencyMap((Map(), Map()), steps)
  val initialSteps = findStartingSteps()
  val queue = Queue(initialSteps: _*)  // queues all members of initialSteps

  def recurses(q: Queue[Char], visited: Queue[Char]): Queue[Char] = {
    if(q.isEmpty) {
      visited
    } else {
      val (label,  tail) = q.dequeue

      val children = dependencyTree._1.getOrElse(label, List[Char]())

      val v = dependencyTree._2.getOrElse(label, Nil) match {
        case Nil =>
          if(visited.contains(label)) visited
          else visited  :+ label
        case requisites =>
          val requisitesComplete = requisites.toSet.subsetOf(visited.toSet)
          if(requisitesComplete) {
            if(visited.contains(label)) visited
            else visited  :+ label
          }
          else visited
      }

      recurses(Queue(children: _*) ++ tail, v)
    }
  }

  recurses(queue, Queue[Char]()).mkString
}

println(s"Part One: ${buildList()}")

//  BFKEGNOVATIHXYZRMCJDLSUPWQ <-
//  BFKEGNOVATIHXZYRMCJDLSUPWQ
//  BFKEGNOVATIHXZYRMCJDLSUPWQ