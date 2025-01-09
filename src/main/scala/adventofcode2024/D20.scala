package adventofcode2024

import adventofcode2024.D16.{Coord, Route, orderRoutes}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object D20 {
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
      Set(Coord(x - 1, y), Coord(x + 1, y), Coord(x, y - 1), Coord(x, y + 1))
    }

    def distance(other: Coord): Int = {
      Math.abs(x - other.x) + Math.abs(y - other.y)
    }
  }

  def getNthNeighbours(n: Int, curCoord: Coord): Vector[Coord] = {
    val allCoords = for {
      x <- curCoord.x - n to curCoord.x + n
      y <- curCoord.y - n to curCoord.y + n
    } yield Coord(x,y)

    allCoords.toVector.filter(_.distance(curCoord) <= n)
  }

  case class Route(end: Coord, prev: Coord, score: Int)

  def getRouteCoords(map: Vector[Vector[Char]]): (Set[Coord], Coord, Coord) = {
    val coords = map.indices.toSet.flatMap(x => map(x).indices.map(y => Coord(x, y)))
    val routeCoords = coords.filter(coord => map(coord.x)(coord.y) != '#')
    val endCoord = routeCoords.find(coord => map(coord.x)(coord.y) == 'E').get
    val startCoord = routeCoords.find(coord => map(coord.x)(coord.y) == 'S').get
    (routeCoords, startCoord, endCoord)
  }

  @tailrec
  def cheapestRoute(
    toExplore: mutable.PriorityQueue[Route],
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
      val nextCoords = curCoord.getNeighbours.intersect(routeCoords) - prevCoord
      val nextFoundRoutes = nextCoords.foldLeft(foundRoutes) { case (curFoundRoutes, newCoord) =>
        val newScore = curScore + 1
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

  @tailrec
  def rollBackRoute(curBest: Vector[Coord], routes: Map[Coord, Route], startCoord: Coord): Vector[Coord] = {
    val prev = routes(curBest.head).prev
    if (prev == startCoord) startCoord +: curBest
    else rollBackRoute(prev +: curBest, routes, startCoord)
  }

  val orderRoutes = new Ordering[Route] {
    override def compare(x: Route, y: Route): Int = x.score - y.score
  }

  def prettyPrint(route: Set[Coord], map: Vector[Vector[Char]]): Unit = {
    val updatedMap =
      map
        .zipWithIndex
        .map((row, x) => row.zipWithIndex.map((char, y) => if (route.contains(Coord(x, y))) 'O' else char).mkString(""))
        .mkString("\n")
    println(updatedMap)
  }

  def calcBestRoute(map: Vector[Vector[Char]],
    routeCoords: Set[Coord],
    startCoord: Coord,
    endCoord: Coord
  ): Vector[Coord] = {
    val toExplore = mutable.PriorityQueue(Route(startCoord, startCoord - Coord(0, 1), 0))(orderRoutes)
    val foundRoutes = Map[Coord, Route]()
    val (originalRoute, originalDist) = cheapestRoute(toExplore, routeCoords, foundRoutes, startCoord, endCoord)
    originalRoute
  }

  def d20T1(
    map: Vector[Vector[Char]], routeCoords: Set[Coord], startCoord: Coord, endCoord: Coord): Int = {
    val originalRoute = calcBestRoute(map, routeCoords, startCoord, endCoord)
    val originalDist = originalRoute.size - 1
    val distanceFromStart = originalRoute.zipWithIndex.toMap
    val distanceToEnd = distanceFromStart.view.mapValues(curDist => originalDist - curDist).toMap
    val cheatDistances = distanceFromStart.toVector.flatMap { case (coord, prevDistance) =>
      coord.getNeighbours.flatMap(_.getNeighbours).filter(distanceToEnd.contains).map(distanceToEnd(_) + prevDistance + 2)
    }
    cheatDistances.count(_ <= originalDist - 100)
  }

  def d20T2(map: Vector[Vector[Char]], routeCoords: Set[Coord], startCoord: Coord, endCoord: Coord): Int = {
    val originalRoute = calcBestRoute(map, routeCoords, startCoord, endCoord)
    val originalDist = originalRoute.size - 1
    val distanceFromStart = originalRoute.zipWithIndex.toMap
    val distanceToEnd = distanceFromStart.view.mapValues(curDist => originalDist - curDist).toMap
    val cheatDistances = distanceFromStart.toVector.flatMap { case (curCoord, prevDistance) =>
      val neighbours = getNthNeighbours(20, curCoord)
      val filteredNeighbours = neighbours.filter(distanceToEnd.contains)
      val res = filteredNeighbours.map(curEnd => distanceToEnd(curEnd) + prevDistance + curCoord.distance(curEnd)).filter(_ < originalDist)
      res
    }
    cheatDistances.count(_ <= originalDist - 100)
  }

  def main(args: Array[String]): Unit = {
    val parsedMap = parseD16("d20.txt")
    val (routeCoords, startCoord, endCoord) = getRouteCoords(parsedMap)
    val d20t1 = d20T1(parsedMap, routeCoords, startCoord, endCoord)
    val d20t2 = d20T2(parsedMap, routeCoords, startCoord, endCoord)
    assert(d20t1 == 1395)
    assert(d20t2 == 993178)
  }
}
