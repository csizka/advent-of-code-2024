package adventofcode2024

import scala.annotation.targetName
import scala.io.Source

object D15 {

  case class Coord(x: Int, y: Int) {
    def +(otherCoord: Coord): Coord = {
      Coord(otherCoord.x + x, otherCoord.y + y)
    }
    def getChar(map: Vector[Vector[Char]]): Char = {
      map(x)(y)
    }

    def charEquals(map: Vector[Vector[Char]], char: Char): Boolean = {
      map(x)(y) == char
    }

    def charIsInSet(map: Vector[Vector[Char]], set: Set[Char]): Boolean = {
      set.contains(map(x)(y))
    }
  }
  def parseD15(path: String): (Vector[Vector[Char]], Vector[(Int, Int)]) = {
    val source = Source.fromResource(path)
    val allInput = source.getLines().toVector
    source.close()
    val validMapChars = Set('#', '.', 'O', '@')
    val validDirChars = Set('<', '^', '>', 'v')
    val map = allInput.filter(row => row.forall(validMapChars.contains)).map(_.toCharArray.toVector).filterNot(_.isEmpty)
    val steps = allInput.filter(row => row.forall(validDirChars.contains)).flatMap(_.toCharArray).map {
      case '<' => (0,-1)
      case '^' => (-1,0)
      case 'v' => (1,0)
      case '>' => (0,1)
    }
    (map, steps)
  }

  def getRobotCoord(map: Vector[Vector[Char]]): Option[(Int, Int)] = {
    map.indices.flatMap(x => map(x).indices.toVector.flatMap(y => Vector((x, y)))).find((x, y) => map(x)(y) == '@')
  }

  def findClosestCoord(map: Vector[Vector[Char]], step: (Int, Int), obj: Char, robotStartCoord: (Int, Int)): Option[(Int, Int)] =  {
    val (row, col) = robotStartCoord
    step match {
      case (0,-1) => (0 until col).findLast(ix => map(row)(ix) == obj).fold(None)(Some(row, _))
      case (-1,0) => (0 until row).findLast(ix => map(ix)(col) == obj).fold(None)(Some(_, col))
      case (1,0) => (row until map.size).find(ix => map(ix)(col) == obj).fold(None)(Some(_, col))
      case (0,1) => (col until map(row).size).find(ix => map(row)(ix) == obj).fold(None)(Some(row, _))
    }
  }

  def moveStep(step: (Int, Int), map: Vector[Vector[Char]], robotStartCoord: (Int, Int)): (Vector[Vector[Char]], (Int, Int)) = {
    val (robotX, robotY) = robotStartCoord
    val closestWallCoord = findClosestCoord(map, step, '#', robotStartCoord)
    val closestFreeCoord = findClosestCoord(map, step, '.', robotStartCoord)
    (closestWallCoord, closestFreeCoord) match {
      case (_, None) => (map, robotStartCoord)
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._1 < 0 && wallX > freeX => (map, robotStartCoord)
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._1 < 0 && wallX < freeX =>
        val freeCoordUpdated = map.updated(freeX, map(freeX).updated(freeY, 'O'))
        val oldRobotCoordUpdated = freeCoordUpdated.updated(robotX, freeCoordUpdated(robotX).updated(robotY, '.'))
        (oldRobotCoordUpdated.updated(robotX - 1, oldRobotCoordUpdated(robotX - 1).updated(robotY, '@')), (robotX + step._1, robotY + step._2))
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._1 > 0 && wallX < freeX => (map, robotStartCoord)
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._1 > 0 && wallX > freeX =>
        val freeCoordUpdated = map.updated(freeX, map(freeX).updated(freeY, 'O'))
        val oldRobotCoordUpdated = freeCoordUpdated.updated(robotX, freeCoordUpdated(robotX).updated(robotY, '.'))
        (oldRobotCoordUpdated.updated(robotX + 1, oldRobotCoordUpdated(robotX + 1).updated(robotY, '@')), (robotX + step._1, robotY + step._2))
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._2 > 0 && wallY < freeY => (map, robotStartCoord)
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._2 > 0 && wallY > freeY =>
        val freeCoordUpdated = map.updated(freeX, map(freeX).updated(freeY, 'O'))
        val oldRobotCoordUpdated = freeCoordUpdated.updated(robotX, freeCoordUpdated(robotX).updated(robotY, '.'))
        (oldRobotCoordUpdated.updated(robotX, oldRobotCoordUpdated(robotX).updated(robotY + 1, '@')), (robotX + step._1, robotY + step._2))
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._2 < 0 && wallY > freeY => (map, robotStartCoord)
      case (Some(wallX, wallY), Some(freeX, freeY)) if step._2 < 0 && wallY < freeY =>
        val freeCoordUpdated = map.updated(freeX, map(freeX).updated(freeY, 'O'))
        val oldRobotCoordUpdated = freeCoordUpdated.updated(robotX, freeCoordUpdated(robotX).updated(robotY, '.'))
        (oldRobotCoordUpdated.updated(robotX, oldRobotCoordUpdated(robotX).updated(robotY - 1, '@')), (robotX + step._1, robotY + step._2))
    }
  }

  def moveAllSteps(steps: Vector[(Int, Int)], startMap: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val startRobotCoord = getRobotCoord(startMap).get
    val (finalMap, finalRobotCoord) = steps.foldLeft((startMap,  startRobotCoord)) { case ((curMap,  curRobotCoord), curStep) =>
    moveStep(curStep, curMap, curRobotCoord)
    }
    finalMap
  }

  def d15T1(map: Vector[Vector[Char]], steps: Vector[(Int, Int)]): Int = {
    val finalMap = moveAllSteps(steps, map)
    val finalBoxCoords = finalMap.indices.flatMap(x => finalMap(x).indices.toVector.flatMap(y => Vector((x, y)))).filter((x, y) => finalMap(x)(y) == 'O').toVector
    val gps = finalBoxCoords.map((x, y) => x * 100 + y).sum
    gps
  }

  def widenMap(map: Vector[Vector[Char]]):  Vector[Vector[Char]] = {
    map.map{_.flatMap{
      case char if char == '@' => Vector('@', '.')
      case char if char != 'O' => Vector.fill(2)(char)
      case _ => Vector('[', ']')
    }}
  }

  def updateMap(startMap: Vector[Vector[Char]], robotCoord: Coord, dir: Coord, prevBoxCoords: Vector[Coord]): Vector[Vector[Char]] = {
    val updatesBoxes = prevBoxCoords.foldLeft(startMap){ case (curMap, curCoord) =>
      val curChar = startMap(curCoord.x)(curCoord.y)
      val intermediateMap = curMap.updated(curCoord.x, curMap(curCoord.x).updated(curCoord.y, '.'))
      val newCoord = curCoord.+(dir)
      intermediateMap.updated(newCoord.x, intermediateMap(newCoord.x).updated(newCoord.y, curChar))
    }
    val newRobotCoord = robotCoord.+(dir)
    val intermedMap = updatesBoxes.updated(robotCoord.x, updatesBoxes(robotCoord.x).updated(robotCoord.y, '.'))
    intermedMap.updated(newRobotCoord.x, intermedMap(newRobotCoord.x).updated(newRobotCoord.y, '@'))
  }

  def move(startMap: Vector[Vector[Char]], dir: Coord, prevCoords: Vector[Coord], processedCoords:Vector[Coord], robotCoord: Coord): (Vector[Vector[Char]], Coord) = {
    val newDirectCoords = prevCoords.map(_ + dir)
    val boxCoords = newDirectCoords.filter(_.charIsInSet(startMap, Set('[', ']')))
    val moveIsBlocked = newDirectCoords.exists(_.charEquals(startMap, '#'))
    val moveIsHorizontal = dir.x == 0
    if (moveIsBlocked) {
      (startMap, robotCoord)
    } else if (boxCoords.isEmpty) {
      (updateMap(startMap, robotCoord, dir, prevCoords ++ processedCoords), robotCoord.+(dir))
    } else if (moveIsHorizontal) {
      move(startMap, dir, boxCoords, prevCoords ++ processedCoords, robotCoord)
    } else {
      val allBoxCoords = boxCoords.toSet.flatMap {
        case coord if coord.getChar(startMap) == ']' => Set(coord, coord.+(Coord(0, -1)))
        case coord if coord.getChar(startMap) == '[' => Set(coord, coord.+(Coord(0, 1)))
      }.toVector ++ boxCoords
      move(startMap, dir, allBoxCoords, prevCoords ++ processedCoords, robotCoord)
    }
  }

  def moveAllStepsV2(steps: Vector[Coord], startMap: Vector[Vector[Char]]): Vector[Vector[Char]] = {
    val startRobotCoord = getRobotCoord(startMap).get
    val robotCoord = Coord(startRobotCoord._1, startRobotCoord._2)
    val (finalMap, finalRobotCoord) = steps.foldLeft((startMap, robotCoord)) { case ((curMap, curRobotCoord), curStep) =>
      move(curMap, curStep, Vector(curRobotCoord), Vector(), curRobotCoord)
    }
    finalMap
  }

  def d15T2(map: Vector[Vector[Char]], steps: Vector[(Int, Int)]): Int = {
    val dirs = steps.map(Coord(_,_))
    val finalMap = moveAllStepsV2(dirs, map)
    val finalBoxCoords = finalMap.indices.flatMap(x => finalMap(x).indices.toVector.flatMap(y => Vector((x, y)))).filter((x, y) => finalMap(x)(y) == '[').toVector
    val gps = finalBoxCoords.map((x, y) => x * 100 + y).sum
    gps
  }

  def prettyPrint(map: Vector[Vector[Char]]): Unit = {
    println(map.map{_.mkString("")}.mkString("\n"))
  }

  def main(args: Array[String]): Unit = {
    val (parsedMap, parsedMovements) = parseD15("d15.txt")
    val d15t1 = d15T1(parsedMap, parsedMovements)
    val wideMap = widenMap(parsedMap)
    val d15t2 = d15T2(wideMap, parsedMovements)
    assert(d15t1 == 1430536)
    assert(d15t2 == 1452348)
  }
}
