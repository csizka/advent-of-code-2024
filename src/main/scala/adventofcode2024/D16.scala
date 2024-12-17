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
  def cheapestRoutes(
    toExplore: PriorityQueue[Route],
    routeCoords: Set[Coord],
    foundRoutes: Map[Coord, Set[Route]],
    startCoord: Coord,
    endCoord: Coord
  ): (Set[Coord], Int) = {
    if (toExplore.isEmpty)
      val endMinScore = foundRoutes(endCoord).minBy(_.score).score
      val finalFoundRoutes = foundRoutes + (endCoord -> foundRoutes(endCoord).filter(_.score == endMinScore)) + (startCoord -> Set())
      println("")
      (rollBackAllRoutes(Set(endCoord), Set[Coord](), finalFoundRoutes, startCoord), foundRoutes(endCoord).head.score)
    else {
      val curRoute = toExplore.dequeue()
      val curCoord = curRoute.end
      val prevCoord = curRoute.prev
      val curScore = curRoute.score
      val curDir = curCoord - prevCoord
      val nextCoords = curCoord.getNeighbours.intersect(routeCoords) - prevCoord
      val nextFoundRoutes = nextCoords.foldLeft(foundRoutes) { case (curFoundRoutes, newCoord) =>
        val newDir = newCoord - curCoord
        val scoreToAdd = if (newDir == curDir) 1 else 1001
        val newScore = curScore + scoreToAdd
        foundRoutes.get(newCoord) match {
          case Some(oldRoutes) if oldRoutes.head.score < newScore && oldRoutes.head.score + 1000 != newScore => curFoundRoutes
          case Some(oldRoutes) if oldRoutes.minBy(_.score).score == newScore ||
            oldRoutes.minBy(_.score).score + 1000 == newScore ||
            oldRoutes.minBy(_.score).score - 1000 == newScore =>
            val newRoute = Route(newCoord, curCoord, newScore)
            toExplore.enqueue(newRoute)
            curFoundRoutes + (newCoord -> (curFoundRoutes.getOrElse(newCoord, Set()) + newRoute))
          case _ =>
            val newRoute = Route(newCoord, curCoord, newScore)
            toExplore.enqueue(newRoute)
            curFoundRoutes + (newCoord -> Set(newRoute))
        }
      }
      cheapestRoutes(toExplore, routeCoords, nextFoundRoutes, startCoord, endCoord)
    }
  }

  @tailrec
  def rollBackAllRoutes(curCoords: Set[Coord], done: Set[Coord], routes: Map[Coord, Set[Route]], startCoord: Coord): Set[Coord] = {
    val prevs = curCoords.flatMap(coord => routes(coord).map(_.prev))
    if (curCoords.isEmpty)
      done
    else
      rollBackAllRoutes(prevs, done ++ curCoords, routes, startCoord)
  }

  def d16T2(routeCoords: Set[Coord], startCoord: Coord, endCoord: Coord, map: Vector[Vector[Char]]): Int = {
    val toExplore = mutable.PriorityQueue(Route(startCoord, startCoord - Coord(0, 1), 0))(orderRoutes)
    val foundRoutes = Map[Coord, Set[Route]]()
    val (finalRouteV1, finalScoreV1) = cheapestRoutes(toExplore, routeCoords, foundRoutes, startCoord, endCoord)
//    prettyPrint(finalRouteV1, map)
    finalRouteV1.size
  }

  def d16(): Unit = {
    val map = parseD16("d16.txt")
    val (routeCoords, startCoord, targetCoord) = getRouteCoords(map)
//    val d16t1 = d16T1(routeCoords, startCoord, targetCoord)
    val d16t2 = d16T2(routeCoords, startCoord, targetCoord, map)

//    println(d16t1)
    println(d16t2)
  }

  def main(args: Array[String]): Unit = {

    d16()

  }


}
