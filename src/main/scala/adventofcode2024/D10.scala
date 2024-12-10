package adventofcode2024
import D6.parseMap

import scala.annotation.tailrec

object D10 {

  def findStartCoords(map: Vector[Vector[Int]], maxX: Int, maxY: Int): List[(Int, Int)] = {
    (for {
      x <- 0 to maxX
      y <- 0 to maxY
    } yield (x,y)).filter(map(_)(_) == 0).toList
  }

  @tailrec
  def countScore(curCoord: Set[(Int, Int)], curNum: Int, map: Vector[Vector[Int]], maxX: Int , maxY: Int): Int = {
    val nextCoords = curCoord.flatMap((x,y) => Set((x - 1,y), (x + 1,y), (x,y - 1), (x,y + 1))
        .filter((curX,curY) => curX >= 0 && curX <= maxX && curY >= 0 && curY <= maxY && map(curX)(curY) == curNum + 1))
    if (curNum == 8) nextCoords.size
    else countScore(nextCoords, curNum + 1, map, maxX, maxY)
  }
  
  @tailrec
  def countRating(curCoord: List[(Int, Int)], curNum: Int, map: Vector[Vector[Int]], maxX: Int , maxY: Int): Int = {
    val nextCoords = curCoord.flatMap((x,y) => List((x - 1,y), (x + 1,y), (x,y - 1), (x,y + 1))
      .filter((curX,curY) => curX >= 0 && curX <= maxX && curY >= 0 && curY <= maxY && map(curX)(curY) == curNum + 1))
    if (curNum == 8) nextCoords.size
    else countRating(nextCoords, curNum + 1, map, maxX, maxY)
  }
  
  def d10T1(map: Vector[Vector[Int]], maxX: Int, maxY: Int): Int = {
    val zeroCoords = findStartCoords(map, maxX, maxY)
    zeroCoords.map(coord => countScore(Set(coord), 0, map, maxX, maxY)).sum
  }

  def d10T2(map: Vector[Vector[Int]], maxX: Int, maxY: Int): Int = {
    val zeroCoords = findStartCoords(map, maxX, maxY)
    zeroCoords.map(coord => countRating(List(coord), 0, map, maxX, maxY)).sum
  }

  def printD10(): Unit = {
    val map = parseMap("d10.txt").map(_.map(_.toInt - 48))
    val maxX = map.size - 1
    val maxY = map(0).size - 1
    val d10t1 = d10T1(map, maxX, maxY)
    val d10t2 = d10T2(map, maxX, maxY)
    println(d10t1)
    println(d10t2)
  }
}
