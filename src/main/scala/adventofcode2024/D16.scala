package adventofcode2024
import adventofcode2024.D15.Coord

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.io.Source

object D16 {

  def parseD16(path: String): Vector[Vector[Char]] = {
    def source = Source.fromResource(path)
    val map = source.getLines().toVector.map(_.toCharArray.toVector)
    source.close()
    map
  }

  case class Coord(x: Int, y: Int) {
    def +(otherCoord: Coord): Coord = {
      Coord(otherCoord.x + x, otherCoord.y + y)
    }

    def -(otherCoord: Coord): Coord = {
      Coord(x - otherCoord.x, y - otherCoord.y)
    }

    def getNeighbours: Set[Coord] = {
      Set(Coord(x - 1,y), Coord(x + 1,y), Coord(x,y - 1), Coord(x,y + 1))
    }
  }
  case class Route(end: Coord, prev: Coord, score: Int)

  case class State(coord: Coord, dir: Coord)

  case class StateRoute(end: State, prev: State, score: Int)

  def getRouteCoords(map: Vector[Vector[Char]]): (Set[Coord], Coord, Coord) = {
    val coords = map.indices.toSet.flatMap(x => map(x).indices.map( y => Coord(x, y)))
    val routeCoords = coords.filter(coord => map(coord.x)(coord.y) != '#')
    val endCoord = routeCoords.find(coord => map(coord.x)(coord.y) == 'E').get
    val startCoord = routeCoords.find(coord => map(coord.x)(coord.y) == 'S').get
    (routeCoords, startCoord, endCoord)
  }

  @tailrec
  def rollBackRoute(curBest: Vector[Coord], routes: Map[Coord, Route], startCoord: Coord): Vector[Coord] = {
    val prev = routes(curBest.head).prev
    if (prev == startCoord) startCoord +: curBest
    else rollBackRoute(prev +: curBest, routes, startCoord)
  }

  @tailrec
  def cheapestRoute(
    toExplore: PriorityQueue[Route],
    routeCoords: Set[Coord],
    foundRoutes: Map[Coord, Route],
    startCoord: Coord,
    endCoord: Coord
  ): (Vector[Coord], Int) = {
    if (toExplore.isEmpty) (rollBackRoute(Vector(endCoord), foundRoutes, startCoord), foundRoutes(endCoord).score)
    else {
      val curRoute = toExplore.dequeue()
      def curCoord = curRoute.end
      val prevCoord = curRoute.prev
      val curScore = curRoute.score
      val curDir = curCoord - prevCoord
      val nextCoords = curCoord.getNeighbours.intersect(routeCoords) - prevCoord
      val nextFoundRoutes = nextCoords.foldLeft(foundRoutes){ case (curFoundRoutes, newCoord) =>
        val newDir = newCoord - curCoord
        val scoreToAdd = if (newDir == curDir) 1 else 1001
        val newScore = curScore + scoreToAdd
        foundRoutes.get(newCoord) match {
          case Some(oldRoute) if oldRoute.score < newScore => curFoundRoutes
          case _ =>
            val newRoute = Route(newCoord, curCoord, newScore)
            toExplore.enqueue(newRoute)
            curFoundRoutes + (newCoord -> newRoute)
        }
      }
      cheapestRoute(toExplore, routeCoords, nextFoundRoutes, startCoord, endCoord)
    }
  }

  val orderRoutes = new Ordering[Route] {
    override def compare(x: Route, y: Route): Int = x.score - y.score
  }

  val orderStateRoutes = new Ordering[StateRoute] {
    override def compare(x: StateRoute, y: StateRoute): Int = x.score - y.score
  }

  def d16T1(routeCoords: Set[Coord], startCoord: Coord, endCoord: Coord): Int = {
    val toExplore = mutable.PriorityQueue(Route(startCoord, startCoord - Coord(0, 1), 0))(orderRoutes)
    val foundRoutes = Map[Coord, Route]()
    val (finalRoute, finalScore) = cheapestRoute(toExplore, routeCoords, foundRoutes, startCoord, endCoord)
    finalScore
  }

  def prettyPrint(route: Set[Coord], map: Vector[Vector[Char]]): Unit = {
    val updatedMap =
      map
        .zipWithIndex
        .map((row, x) => row.zipWithIndex.map((char, y) =>  if (route.contains(Coord(x, y))) 'O' else char).mkString(""))
        .mkString("\n")
    println(updatedMap)
  }

  @tailrec
  def rollBackAllRoutes(curStates: Set[State], done: Set[Coord], routes: Map[State, Set[StateRoute]], startCoord: Coord): Set[Coord] = {
    val prevs = curStates.flatMap(state => routes(state).map(_.prev))
    if (curStates.isEmpty)
      done
    else
      val newDone = done ++ curStates.map(_.coord)
      rollBackAllRoutes(prevs, newDone, routes, startCoord)
  }

  def d16T2(routeCoords: Set[Coord], startCoord: Coord, endCoord: Coord, map: Vector[Vector[Char]]): Int = {
    val startState = State(startCoord, Coord(0,1))
    val toExplore = mutable.PriorityQueue(StateRoute(startState, State(startCoord + Coord(0, -1), Coord(0,1)), 0))(orderStateRoutes)
    val foundRoutes = Map[State, Set[StateRoute]]()
    val finalRoute = cheapestRoutes(toExplore, routeCoords, foundRoutes, startCoord, endCoord, None)
    finalRoute.size
  }

  @tailrec
  def cheapestRoutes(
    toExplore: PriorityQueue[StateRoute],
    routeCoords: Set[Coord],
    foundRoutes: Map[State, Set[StateRoute]],
    startCoord: Coord,
    endCoord: Coord,
    foundMin: Option[Int]
  ): Set[Coord]= {
    if (toExplore.isEmpty)
      val startState = State(startCoord, Coord(0,1))
      val finalFoundRoutes = foundRoutes + (startState -> Set[StateRoute]())
      val endRoues =
        Set(Coord(0, -1), Coord(0, 1), Coord(-1, 0), Coord(1, 0))
          .map(State(endCoord, _))
          .intersect(foundRoutes.keySet)
      val minFinalEndRouteScore = endRoues.map(finalFoundRoutes(_).head.score).min
      val finalEndRoutes = endRoues.filter(finalFoundRoutes(_).head.score == minFinalEndRouteScore)
      rollBackAllRoutes(finalEndRoutes, Set[Coord](), finalFoundRoutes, startCoord)
    else {
      val curStateRoute = toExplore.dequeue()
      val curState = curStateRoute.end
      val prevState = curStateRoute.prev
      val curScore = curStateRoute.score
      val curDir = curState.dir
      val nextCoords = curState.coord.getNeighbours.intersect(routeCoords) - prevState.coord
      val (nextFoundStateRoutes, newMinOpt) = nextCoords.foldLeft(foundRoutes, foundMin) { case ((curFoundRoutes, curMinOpt), newCoord) =>
        val newDir = newCoord - curState.coord
        val scoreToAdd = if (newDir == curDir) 1 else 1001
        val newScore = curScore + scoreToAdd
        curMinOpt match {
          case Some(curMin) if curMin < newScore => (curFoundRoutes, curMinOpt)
          case _ =>
            val newState = State(newCoord, newDir)
            foundRoutes.get(newState) match {
              case _ if newCoord == endCoord =>
                val newRoute = StateRoute(newState, curState, newScore)
                toExplore.enqueue(newRoute)
                (curFoundRoutes + (newState -> Set(newRoute)), Some(newScore))
              case Some(oldRoutes) if oldRoutes.head.score == newScore =>
                val newRoute = StateRoute(newState, curState, newScore)
                toExplore.enqueue(newRoute)
                (curFoundRoutes + (newState -> (curFoundRoutes.getOrElse(newState, Set()) + newRoute)), curMinOpt)
              case Some(oldRoutes) if oldRoutes.head.score < newScore =>
                (curFoundRoutes, curMinOpt)
              case _ =>
                val newRoute = StateRoute(newState, curState, newScore)
                toExplore.enqueue(newRoute)
                (curFoundRoutes + (newState -> Set(newRoute)), curMinOpt)
            }
        }

      }
      cheapestRoutes(toExplore, routeCoords, nextFoundStateRoutes, startCoord, endCoord, newMinOpt)
    }
  }

  def d16(): (Int, Int) = {
    val map = parseD16("d16.txt")
    val (routeCoords, startCoord, targetCoord) = getRouteCoords(map)
    val d16t1 = d16T1(routeCoords, startCoord, targetCoord)
    val d16t2 = d16T2(routeCoords, startCoord, targetCoord, map)

    (d16t1,d16t2)
  }





  def main(args: Array[String]): Unit = {

    d16()

  }


}
