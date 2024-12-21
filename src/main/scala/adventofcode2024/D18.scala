package adventofcode2024

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import D16.*

object D18 {

  case class Cost(end: Coord, score: Int)

  def parseD18(path: String): Vector[Coord] = {
    val numRegex = "\\d+".r
    val source = Source.fromResource(path)
    val fallenCoords = source.getLines().toVector
    source.close()
    fallenCoords.map(coord => coord.split(",")).map{ case Array(x,y) => Coord(y.toInt, x.toInt)}
  }

  def getRouteCoords(blocked: Set[Coord], maxIx: Int): Set[Coord] = {
    (0 to maxIx).toSet.flatMap(x => (0 to maxIx).toVector.map(y => Coord(x, y))).diff(blocked)
  }

  def createMap(fallenCoords: Set[Coord], maxIx: Int): Vector[Vector[Char]] = {
    (0 to maxIx).toVector.map(x => (0 to maxIx).toVector.map(y =>
      if (fallenCoords.contains(Coord(x,y))) '#'
      else if (x == 0 && y == 0) 'S'
      else if (x == maxIx && y == maxIx) 'E'
      else '.'))
  }

  @tailrec
  def shortestRoute(
    toExplore: mutable.PriorityQueue[Cost],
    routeCoords: Set[Coord],
    visited: Set[Coord],
    foundRoutes: Map[Coord, Int],
    endCoord: Coord
  ): Int = {
    if (toExplore.isEmpty) foundRoutes.getOrElse(endCoord, 0)
    else {
      val curRoute = toExplore.dequeue()
      def curCoord = curRoute.end
      val curScore = curRoute.score
      val nextCoords = curCoord.getNeighbours.intersect(routeCoords) -- visited
      val nextFoundRoutes = nextCoords.foldLeft(foundRoutes) { case (curFoundRoutes, newCoord) =>
        val newScore = curScore + 1
        foundRoutes.get(newCoord) match {
          case Some(oldScore) if oldScore < newScore => curFoundRoutes
          case _ =>
            val newRoute = Cost(newCoord, newScore)
            toExplore.enqueue(newRoute)
            curFoundRoutes + (newCoord -> newScore)
        }
      }
      shortestRoute(toExplore, routeCoords, visited ++ nextCoords, nextFoundRoutes, endCoord)
    }
  }

  val orderCosts = new Ordering[Cost] {
    override def compare(x: Cost, y: Cost): Int = y.score - x.score
  }

  def d18T1(fallenCoords: Set[Coord], maxIx: Int, end: Coord): Int = {
    val map = createMap(fallenCoords, maxIx)
    val startQueue = mutable.PriorityQueue[Cost](Cost(Coord(0,0), 0))(orderCosts)
    val routeCoords = getRouteCoords(fallenCoords, maxIx)
    shortestRoute(startQueue, routeCoords, Set(Coord(0,0)), Map[Coord, Int](), end)
  }

  @tailrec
  def d18T2(blocked: Vector[Coord], maxIx: Int, end: Coord, time: Int): Coord = {
    val fallenCoords = blocked.take(time).toSet
    if (d18T1(fallenCoords, maxIx, end) == 0) blocked(time-1)
    else d18T2(blocked, maxIx, end, time + 1)
  }

  @tailrec
  def d18T2V2(blocked: Vector[Coord], maxIx: Int, end: Coord, time: Int): Coord = {
    val fallenCoords = blocked.take(time).toSet
    val routeCoords = getRouteCoords(fallenCoords, maxIx)
    val startQueue = mutable.PriorityQueue[Route](Route(Coord(0,0), Coord(0,0), 0))(orderRoutes)
    if (d18T1(fallenCoords, maxIx, end) == 0) blocked(time-1)
    else {
      val curShortestCoords = cheapestRoute(startQueue, routeCoords, Map[Coord, Route](), Coord(0,0), Coord(70,70))._1.toSet
      val nextCoord = blocked.find(curShortestCoords.contains).get
      val nextTime = blocked.indexOf(nextCoord) + 1
      d18T2V2(blocked, maxIx, end, nextTime)
    }

  }

  def prettyPrint(map: Vector[Vector[Char]]): Unit = {
    println(map.map(_.mkString("")).mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val wallCoords = parseD18("d18.txt")
    val time = 1024
    val maxIx = 70
    val endCoord = Coord(maxIx,maxIx)
    val fallenCoords = wallCoords.take(time).toSet
    val d18t1 = d18T1(fallenCoords, maxIx, endCoord)
    val d18t2 = d18T2(wallCoords, maxIx, endCoord, 2929)
    val d18t2V2 = d18T2V2(wallCoords, maxIx, endCoord, 0)
//    assert(d18t2 == Coord(64,44) && d18t1 == 232)
    println(d18t2V2)
  }
}
