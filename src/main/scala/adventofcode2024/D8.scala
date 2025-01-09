package adventofcode2024

import scala.io.Source

object D8 {
  def parseD8(path: String): Vector[Vector[Char]] = {
    Source.fromResource(path).getLines.toVector.map(_.toVector.map { case n if n == '#' => '.' case n => n })
  }

  def antennaCoords(map: Vector[Vector[Char]]): Map[Char, Set[(Int, Int)]] = {
    (for {
      x <- map.indices
      y <- map(x).indices
    } yield (x, y)).foldLeft(Map[Char, Set[(Int, Int)]]()) { case (curMap, (curX, curY)) =>
      val curChar = map(curX)(curY)
      if (curChar != '.') curMap + (curChar -> (curMap.getOrElse(curChar, Set()) + (curX -> curY)))
      else curMap
    }
  }

  def collectSimilarNodes(antennaCoords: Set[(Int, Int)], map: Vector[Vector[Char]], maxX: Int, maxY: Int): Set[(Int, Int)] = {
    val antennaVector = antennaCoords.toVector
    (for {
      fstIx <- antennaVector.indices
      sndIx <- fstIx + 1 until antennaVector.size
    } yield (antennaVector(fstIx), antennaVector(sndIx))).foldLeft(Set[(Int, Int)]()) { case (curSet, ((lhsX, lhsY), (rhsX, rhsY))) =>
      val fstAntenna = (lhsX + (lhsX - rhsX), lhsY + (lhsY - rhsY))
      val sndAntenna = (rhsX + (rhsX - lhsX), rhsY + (rhsY - lhsY))
      val nextSet = Set(fstAntenna, sndAntenna). filter((x, y) => x >= 0 && x <= maxX && y >= 0 && y <= maxY )
      curSet ++ nextSet
    }
  }

  def collectSimilarNodesV2(antennaCoords: Set[(Int, Int)], map: Vector[Vector[Char]], maxX: Int, maxY: Int): Set[(Int, Int)] = {
    val antennaVector = antennaCoords.toVector.sortBy(_._1)
    (for {
      fstIx <- antennaVector.indices
      sndIx <- fstIx + 1 until antennaVector.size
    } yield (antennaVector(fstIx), antennaVector(sndIx))).foldLeft(Set[(Int, Int)]()) { case (curSet, ((lhsX, lhsY), (rhsX, rhsY))) =>
      val (xDiff, yDiff) = (rhsX - lhsX, rhsY - lhsY)
      val yIsIncreasing = yDiff > 0
      val (min, max) =
        if (yIsIncreasing) {
          (List(lhsX / xDiff, lhsY / yDiff).min, List((maxX - lhsX) / xDiff,(maxY - lhsY) / yDiff).min)
        } else {
          (List(lhsX / xDiff , (maxY - lhsY) / (-yDiff)).min, List((maxX - lhsX) / xDiff, lhsY / (-yDiff)).min)
        }
      val nextSet = (- min to max).toSet.map(n => (lhsX + (n * xDiff) , lhsY + (n * yDiff)))
      curSet ++ nextSet
    }
  }

  def collectAllNodes(antennaCoords: Map[Char, Set[(Int, Int)]], map: Vector[Vector[Char]]): Set[(Int, Int)] = {
    val maxX = map.size - 1
    val maxY = map(0).size - 1
    antennaCoords.foldLeft(Set[(Int, Int)]()){ case (curSet, (antennaType, coords)) =>
      curSet ++ collectSimilarNodes(coords, map, maxX, maxY)}
  }

  def collectAllNodesV2(antennaCoords: Map[Char, Set[(Int, Int)]], map: Vector[Vector[Char]]): Set[(Int, Int)] = {
    val maxX = map.size - 1
    val maxY = map(0).size - 1
    antennaCoords.foldLeft(Set[(Int, Int)]()) { case (curSet, (antennaType, coords)) =>
      curSet ++ collectSimilarNodesV2(coords, map, maxX, maxY)
    }
  }

  def collectAntennaNodes(antennaCoords: Map[Char, Set[(Int, Int)]], map: Vector[Vector[Char]]): Set[(Int, Int)] = {
    val maxX = map.size - 1
    val maxY = map(0).size - 1
    antennaCoords.foldLeft(Set[(Int, Int)]()) { case (curSet, (antennaType, coords)) =>
      curSet ++ collectSimilarNodes(coords, map, maxX, maxY)
    }
  }

  def main(args: Array[String]): Unit = {
    val parsedMap = parseD8("d8.txt")
    val antennas = antennaCoords(parsedMap)
    val d8t1 = collectAllNodes(antennas, parsedMap).size
    val d8t2 = collectAllNodesV2(antennas, parsedMap).size

    assert(d8t1 == 320)
    assert(d8t2 == 1157)
  }
}
