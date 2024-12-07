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
    val coords = for {
      x <- map.indices
      y <- map(0).indices
    } yield (x,y)

    coords.toSet
  }

  def obstacleCoords(guardMap: Vector[Vector[Char]]): Set[(Int, Int)] = {
    allCoords(guardMap).filter(guardMap(_)(_) == '#')
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
    obstacleCoords: Set[(Int, Int)],
    maxRowIx: Int,
    maxColIx: Int,
    guardPosition: ((Int, Int), Char),
    prevPositions: Set[(Int, Int)]): Set[(Int, Int)] = {
    val ((curX, curY), curDir) = guardPosition
    val (nextPosCoords, nextDir) = curDir match {
      case 'u' => (obstacleCoords.filter((x,y) => x < curX && y == curY).map((x, y) =>(x + 1, y)), 'r')
      case 'd' => (obstacleCoords.filter((x,y) => x > curX && y == curY).map((x, y) =>(x - 1, y)), 'l')
      case 'r' => (obstacleCoords.filter((x,y) => x == curX && y > curY).map((x, y) =>(x, y - 1)), 'd')
      case 'l' => (obstacleCoords.filter((x,y) => x == curX && y < curY).map((x, y) =>(x, y + 1)), 'u')
    }

    nextPosCoords.toList match {
      case Nil if curDir == 'u' => prevPositions ++ (0 until curX).map((_, curY))
      case Nil if curDir == 'd' => prevPositions ++ (curX + 1 to maxRowIx).map((_, curY))
      case Nil if curDir == 'r' => prevPositions ++ (curY + 1 to maxColIx).map((curX, _))
      case Nil if curDir == 'l' => prevPositions ++ (0 until curY).map((curX, _))
      case _ if curDir == 'u' =>
        val (nextX, nextY) = nextPosCoords.maxBy(_._1)
        val nextVisited = prevPositions ++ (nextX until curX).map((_, curY))
        collectSteps(obstacleCoords, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case _ if curDir == 'd' =>
        val (nextX, nextY) = nextPosCoords.minBy(_._1)
        val nextVisited = prevPositions ++ (curX + 1 to nextX).map((_, curY))
        collectSteps(obstacleCoords, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case _ if curDir == 'r' =>
        val (nextX, nextY) = nextPosCoords.minBy(_._2)
        val nextVisited = prevPositions ++ (curY to nextY).map((curX, _))
        collectSteps(obstacleCoords, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
      case _ if curDir == 'l' =>
        val (nextX, nextY) = nextPosCoords.maxBy(_._2)
        val nextVisited = prevPositions ++ (nextY until curY).map((curX, _))
        collectSteps(obstacleCoords, maxRowIx, maxColIx, ((nextX, nextY), nextDir), nextVisited)
    }
  }

  @tailrec
  private def hasLoop(
    obstacleCoords: Set[(Int, Int)],
    maxRowIx: Int,
    maxColIx: Int,
    guardPosition: ((Int, Int), Char),
    prevPositions: Map[Char, Set[(Int, Int)]]): Boolean = {
    val ((curX, curY), curDir) = guardPosition
    val (nextPosCoords, nextDir) = curDir match {
      case 'u' => (obstacleCoords.filter((x, y) => x < curX && y == curY).map((x, y) => (x + 1, y)), 'r')
      case 'd' => (obstacleCoords.filter((x, y) => x > curX && y == curY).map((x, y) => (x - 1, y)), 'l')
      case 'r' => (obstacleCoords.filter((x, y) => x == curX && y > curY).map((x, y) => (x, y - 1)), 'd')
      case 'l' => (obstacleCoords.filter((x, y) => x == curX && y < curY).map((x, y) => (x, y + 1)), 'u')
    }

    val nextCoordOption = nextPosCoords.toList match {
      case Nil => None
      case _ if curDir == 'u' => Some(nextPosCoords.maxBy(_._1))
      case _ if curDir == 'd' => Some(nextPosCoords.minBy(_._1))
      case _ if curDir == 'r' => Some(nextPosCoords.minBy(_._2))
      case _ if curDir == 'l' => Some(nextPosCoords.maxBy(_._2))
    }

    nextCoordOption match {
      case None => 
        println("NOT") 
        false
      case Some(nextCoords) =>
        val loopFound = prevPositions.getOrElse(curDir, Set()).contains(nextCoords)
        if (loopFound)
          println("FOUND")
          true
        else
          val nextVisited = prevPositions + (curDir -> (prevPositions.getOrElse(curDir, Set()) + nextCoords))
          hasLoop(obstacleCoords, maxRowIx, maxColIx, (nextCoords, nextDir), nextVisited)
    }
  }

  def printD6(): Unit = {
    val guardMap = parseMap("d6.txt")
    val maxRox = maxRowIx(guardMap)
    val maxCol = maxColIx(guardMap)
    val obsCoords = obstacleCoords(guardMap)
    val startPos = guardPosition(guardMap)
    val routeCoords = collectSteps(obsCoords, maxRox, maxCol, startPos, Set(startPos._1))
    val d6t1 = routeCoords.size
    val d6t2 = (routeCoords - startPos._1).count(coord => hasLoop(obsCoords + coord, maxRox, maxCol, startPos, Map[Char, Set[(Int, Int)]]()))

    println(d6t1)
    println(d6t2)
  }

}
