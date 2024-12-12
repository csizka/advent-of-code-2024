package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D12 {
  def parseD12(path: String): Vector[Vector[Char]] = {
    val bufferedSource = Source.fromResource(path)
    val res = bufferedSource.getLines().toVector.map(_.toVector)
    bufferedSource.close()
    res
  }

  def groupedCoordsByFlower(map: Vector[Vector[Char]]): Map[Char, Set[(Int, Int)]] = {
    val allCoords = (for {
      x <- map.indices
      y <- map(x).indices
    } yield (x,y)).toSet
    allCoords.groupBy(map(_)(_))
  }

  @tailrec
  def calcMetricsForFlower(
    restCoords: Set[(Int, Int)],
    curCoords: Set[(Int, Int)],
    prevCoords: Set[(Int, Int)],
    curPerimeterCoords: Set[((Int, Int), Char)],
    finishedRegions: Vector[(Set[((Int, Int), Char)], Int)],
  ): Vector[(Set[((Int, Int), Char)], Int)] = {
    val neighbours = curCoords.flatMap((x, y) => Set(((x - 1, y), 'd'), ((x + 1, y), 'u'), ((x, y - 1), 'l'), ((x, y + 1), 'r')))
    val nextSameFlowers = neighbours.collect{ case (coord, dir) if restCoords.contains(coord) => coord }
    val newPrevCoords = nextSameFlowers ++ prevCoords
    val foreignNeighbours = neighbours.filterNot((neighbourCoord, neighbourDir) => newPrevCoords.contains(neighbourCoord))
    val newPerimeterCoords = foreignNeighbours ++ curPerimeterCoords

    if (nextSameFlowers.nonEmpty) {
      calcMetricsForFlower(restCoords -- nextSameFlowers, nextSameFlowers, newPrevCoords, newPerimeterCoords, finishedRegions)
    } else if (nextSameFlowers.isEmpty && restCoords.nonEmpty) {
      val nextCoord = restCoords.head
      calcMetricsForFlower(
        restCoords - nextCoord,
        Set(nextCoord),
        Set(nextCoord),
        Set[((Int, Int), Char)](),
        finishedRegions :+ (newPerimeterCoords, newPrevCoords.size))
    } else {
      finishedRegions :+ (newPerimeterCoords, newPrevCoords.size)
    }
  }

  def areNeighbours(fst: (Int, Int), snd: (Int, Int), dir: Char): Boolean = {
    val ((fstX, fstY), (sndX, sndY)) = (fst, snd)
    if (dir == 'h') {
      fstX == sndX && Math.abs(fstY - sndY) == 1
    } else {
      fstY == sndY && Math.abs(fstX - sndX) == 1
    }
  }

  def collectSortedNeighbours(coords: Vector[Vector[(Int, Int)]], dir: Char): Vector[(Int, Int)] = {
    coords.flatMap {
      case head +: rest =>
        val (collectedNeighbours, lastNeighbour) =
          rest.foldLeft(Vector[(Int, Int)](), head) { case ((curCollectedNeighbours, prevCoord), curCoord) =>
            if (areNeighbours(prevCoord, curCoord, dir)) (curCollectedNeighbours :+ prevCoord, curCoord)
            else (curCollectedNeighbours, curCoord)
        }
        collectedNeighbours
      case _ => Vector()
    }
  }

  def calcDiscountPerimeter(neighbourInfo: Set[((Int, Int), Char)]): Int = {
    val originalPerimeter = neighbourInfo.size

    val sameDirMap = neighbourInfo.groupBy(_._2).map{
      case (dir, neighboursWDir) if dir == 'u' || dir == 'd' =>
        val sortedNeighbours = neighboursWDir.map(_._1).toVector.groupBy(_._1).toVector.map { case (col, coords) => coords.sortBy(_._2) }
        collectSortedNeighbours(sortedNeighbours, 'h')
      case (dir, neighboursWDir)  =>
        val sortedNeighbours = neighboursWDir.map(_._1).toVector.groupBy(_._2).toVector.map { case (col, coords) => coords.sortBy(_._1) }
        collectSortedNeighbours(sortedNeighbours, 'v')
    }.flatten

    val connections = sameDirMap.size
    originalPerimeter - connections
  }

  def d12(flowers: Map[Char, Set[(Int, Int)]]): (Int, Int) = {
    val flowerMetrics = flowers.map((flower, coords) =>
      calcMetricsForFlower(coords.tail,
        Set(coords.head),
        Set(coords.head),
        Set[((Int, Int), Char)](),
        Vector[(Set[((Int, Int), Char)], Int)]()))
    val d12t1 = flowerMetrics.foldLeft(0) { case (curPrice, curMetrics) =>
      curMetrics.map((neighbours, area) => neighbours.size * area).sum + curPrice
    }
    val d12t2 = flowerMetrics.foldLeft(0) { case (curPrice, curMetrics) =>
      curMetrics.map{ case (neighbours, area) => calcDiscountPerimeter(neighbours) * area }.sum + curPrice
    }
    (d12t1, d12t2)
  }


  def printD12(): Unit = {
    val map = parseD12("d12.txt")
    val flowers = groupedCoordsByFlower(map)
    val (d12t1, d12t2) = d12(flowers)
    println(d12t1)
    println(d12t2)
  }
}
