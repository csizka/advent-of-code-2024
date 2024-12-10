package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D6 {
  def parseMap(path: String): Vector[Vector[Char]] = {
    val lines = Source.fromResource(path).getLines
    lines.toVector.map(_.toVector)
  }
  def maxRowIx(map: Vector[Vector[Char]]): Int = map.size - 1
  def maxColIx(map: Vector[Vector[Char]]): Int = map(0).size - 1

  def allCoords(map:Vector[Vector[Char]]): Set[(Int, Int)] = {
    (for {
      x <- map.indices
      y <- map(0).indices
    } yield (x,y)).toSet
  }

  def obstacleCoords(guardMap: Vector[Vector[Char]]):(Map[Int,Set[Int]], Map[Int,Set[Int]]) = {
    val coords = allCoords(guardMap).filter(guardMap(_)(_) == '#')
    (coords.groupBy(_._1).map{ case (n, set) => (n, set.map(_._2))}, coords.groupBy(_._2).map{ case (n, set) => (n, set.map(_._1))})
  }

  def guardPosition(guardMap: Vector[Vector[Char]]): ((Int, Int), Char) = {
    val (guardX, guardY) = allCoords(guardMap).filterNot((x,y) => guardMap(x)(y) == '#' || guardMap(x)(y) == '.').head
    val arrow = guardMap(guardX)(guardY)
    val dir =
      if (arrow == '^') 'u'
      else if (arrow == '<') 'l'
      else if (arrow == '>') 'r'
      else 'd'
    ((guardX, guardY), dir)
  }

  @tailrec
  private def collectSteps (
    obstacleCoordsByX: Map[Int, Set[Int]],
    obstacleCoordsByY: Map[Int, Set[Int]],
    maxRowIx: Int,
    maxColIx: Int,
    guardPosition: ((Int, Int), Char),
    prevPositions: Set[(Int, Int)]): Set[(Int, Int)] = {
    val ((curX, curY), curDir) = guardPosition
    val (nextPosCoords, nextDir) = curDir match {
      case 'u' => obstacleCoordsByY.getOrElse(curY, Set()).filter(_ < curX).maxOption.map(_ + 1 -> curY) -> 'r'
      case 'd' => obstacleCoordsByY.getOrElse(curY, Set()).filter(_ > curX).minOption.map(_ - 1 -> curY) -> 'l'
      case 'r' => obstacleCoordsByX.getOrElse(curX, Set()).filter(_ > curY).minOption.map(num => curX -> (num - 1)) -> 'd'
      case 'l' => obstacleCoordsByX.getOrElse(curX, Set()).filter(_ < curY).maxOption.map(num => curX -> (num + 1)) -> 'u'
    }

    nextPosCoords match {
      case None if curDir == 'u' => prevPositions ++ (0 until curX).map((_, curY))
      case None if curDir == 'd' => prevPositions ++ (curX + 1 to maxRowIx).map((_, curY))
      case None if curDir == 'r' => prevPositions ++ (curY + 1 to maxColIx).map((curX, _))
      case None if curDir == 'l' => prevPositions ++ (0 until curY).map((curX, _))
      case Some((nextX, nextY)) if curDir == 'u' =>
        val nextVisited = prevPositions ++ (nextX until curX).map((_, curY))
        collectSteps(obstacleCoordsByX, obstacleCoordsByY, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case Some((nextX, nextY)) if curDir == 'd' =>
        val nextVisited = prevPositions ++ (curX + 1 to nextX).map((_, curY))
        collectSteps(obstacleCoordsByX, obstacleCoordsByY, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case Some((nextX, nextY)) if curDir == 'r' =>
        val nextVisited = prevPositions ++ (curY to nextY).map((curX, _))
        collectSteps(obstacleCoordsByX, obstacleCoordsByY, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case Some((nextX, nextY)) if curDir == 'l' =>
        val nextVisited = prevPositions ++ (nextY until curY).map((curX, _))
        collectSteps(obstacleCoordsByX, obstacleCoordsByY, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
    }
  }
  
  @tailrec
  private def hasLoop(
    obstacleCoordsByX: Map[Int, Set[Int]],
    obstacleCoordsByY: Map[Int, Set[Int]],
    maxRowIx: Int,
    maxColIx: Int,
    guardPosition: ((Int, Int), Char),
    prevPositions: Map[Char, Set[(Int, Int)]]): Boolean = {
    val ((curX, curY), curDir) = guardPosition
    val (nextPosCoordOpt, nextDir) = curDir match {
      case 'u' => obstacleCoordsByY.getOrElse(curY, Set()).filter(_ < curX).maxOption.map(_ + 1 -> curY) -> 'r'
      case 'd' => obstacleCoordsByY.getOrElse(curY, Set()).filter(_ > curX).minOption.map(_ - 1 -> curY) -> 'l'
      case 'r' => obstacleCoordsByX.getOrElse(curX, Set()).filter(_ > curY).minOption.map(num => curX -> (num - 1)) -> 'd'
      case 'l' => obstacleCoordsByX.getOrElse(curX, Set()).filter(_ < curY).maxOption.map(num => curX -> (num + 1)) -> 'u'
    }

    nextPosCoordOpt match {
      case None => false
      case Some(nextCoords) =>
        val loopFound = prevPositions.getOrElse(curDir, Set()).contains(nextCoords)
        if (loopFound) true
        else
          val nextVisited = prevPositions + (curDir -> (prevPositions.getOrElse(curDir, Set()) + nextCoords))
          hasLoop(obstacleCoordsByX, obstacleCoordsByY, maxRowIx, maxColIx, (nextCoords, nextDir), nextVisited)
    }
  }

  def printD6(): Unit = {
    val guardMap = parseMap("d6.txt")
    val maxRox = maxRowIx(guardMap)
    val maxCol = maxColIx(guardMap)
    val (obsCoordsByX, obsCoordsByY) = obstacleCoords(guardMap)
    val startPos = guardPosition(guardMap)
    val routeCoords = collectSteps(obsCoordsByX, obsCoordsByY, maxRox, maxCol, startPos, Set(startPos._1))
    val d6t1 = routeCoords.size
    val d6t2 = (routeCoords - startPos._1).count((x,y) => 
      hasLoop(obsCoordsByX + (x -> (obsCoordsByX.getOrElse(x, Set()) + y)),
        obsCoordsByY + (y -> (obsCoordsByY.getOrElse(y, Set()) + x)),
        maxRox, maxCol, startPos, Map[Char, Set[(Int, Int)]]()))

    println(d6t1)
    println(d6t2)
  }

}
