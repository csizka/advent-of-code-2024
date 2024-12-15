package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D14 {

  case class Robot(curRow: Int, curCol: Int, rowChange: Int, colChange: Int) {
    def enhanceXSec(sec: Int, rowCount: Int, colCount: Int): Robot = {
      val nextX = Math.floorMod(curRow + sec * rowChange, rowCount)
      val nextY = Math.floorMod(curCol + sec * colChange, colCount)
      Robot(nextX, nextY, rowChange, colChange)
    }

    def getQuadrant(rowCount: Int, colCount: Int): Option[Int] = {
      if ((rowCount % 2 == 1 && curRow == rowCount / 2) || (colCount % 2 == 1 && curCol == colCount / 2)) None
      else {
        val xHalf = if (curRow < rowCount / 2) 0 else 1
        val yHalf = if (curCol < colCount / 2) 0 else 1
        Some(xHalf + yHalf * 2)
      }
    }
  }

  val numSearchRegex = "-?\\d+".r

  def parsed14(path: String): Vector[Robot] = {
    val source = Source.fromResource(path)
    val robots = source.getLines().toVector.map { stats => numSearchRegex.findAllIn(stats).toVector.map(_.toInt) match {
      case Vector(startY, startX, yChange, xChange) => Robot(startX, startY, xChange, yChange)
    }
    }
    source.close()
    robots
  }

  def d14T1(robots: Vector[Robot], sec: Int, rowCount: Int, colCount: Int): Int = {
    val quadrants = robots.flatMap(_.enhanceXSec(sec, rowCount, colCount).getQuadrant(rowCount, colCount)).groupBy(identity)
    quadrants.values.map(_.size).product
  }

  def printMap(robots: Vector[Robot], rowCount: Int, colCount: Int): Unit = {
    val coords = robots.map{ robot => (robot.curCol, robot.curRow)}.toSet
    val map =
      (0 until rowCount)
        .toVector
        .map( row => (0 until colCount).toVector.map { col =>  if (coords.contains((row, col))) 'X' else ' '})
        .map(_.mkString(""))
        .mkString("\n")
    println(map)
  }

  @tailrec
  def d14T2(robots: Vector[Robot], rowCount: Int, colCount: Int, curCount: Int): Unit = {
    if (curCount <= 8179) {
      println(curCount)
      printMap(robots, rowCount: Int, colCount: Int)
      val newRobots = robots.map(_.enhanceXSec(101, rowCount, colCount))
      d14T2(newRobots, rowCount, colCount, curCount + 101)
    }
  }

  def d14(): Int = {
    val robots = parsed14("d14.txt")
    val rowCount = 103
    val colCount = 101
    val d14t1 = d14T1(robots, 100, rowCount, colCount)

    d14t1
  }
}
