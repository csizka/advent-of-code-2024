package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D4 {
  //Day4 parser
  def parsePuzzle(path: String): Vector[Vector[Char]] = {
    val lines = Source.fromResource(path).getLines
    lines.toVector.map(_.toVector)
  }

  val xmas = Vector('X', 'M', 'A', 'S')

  def allCoords(v: Vector[Vector[Char]]): Set[(Int, Int)] = {
    v.indices.flatMap(x => v(x).indices.map(y => (x, y))).toSet
  }

  def xCoords(v: Vector[Vector[Char]], allCoords: Set[(Int, Int)]): Set[(Int, Int)] = {
    allCoords.filter(v(_)(_) == 'X')
  }

  def countXmas(v: Vector[Vector[Char]], origin: (Int, Int), allCoords: Set[(Int, Int)]): Int = {
    val (xX, xY) = origin

    @tailrec
    def isXMas(coords: List[(Int, Int)]): Boolean = {
      val (curX, curY) = coords.head
      val (prevX, prevY) = coords.tail.head
      val (nextX, nextY) = (curX + (curX - prevX), curY + (curY - prevY))
      val letterIsCorrect = xmas(coords.size - 1) == v(curX)(curY)
      val foundAllLetters = coords.size == xmas.size
      val nextIsValidCoord = allCoords.contains((nextX, nextY))
      if (letterIsCorrect && foundAllLetters) true
      else if (letterIsCorrect && nextIsValidCoord) isXMas((nextX, nextY) +: coords)
      else false
    }

    val nextCoords = (for {
      x <- xX - 1 to xX + 1
      y <- xY - 1 to xY + 1
    } yield (x, y)).toList.filter(allCoords.contains)
    nextCoords.count((x, y) => isXMas(List((x, y), origin)))
  }

  def aCoords(v: Vector[Vector[Char]], allCoords: Set[(Int, Int)]): Set[(Int, Int)] = {
    allCoords.filter(v(_)(_) == 'A')
  }

  def isXmas(v: Vector[Vector[Char]], origin: (Int, Int), allCoords: Set[(Int, Int)]): Boolean = {
    val (xX, xY) = origin
    val fstCoords = List((xX - 1, xY - 1), (xX + 1, xY + 1))
    val sndCoords = List((xX - 1, xY + 1), (xX + 1, xY - 1))
    if (fstCoords.forall(allCoords.contains) && sndCoords.forall(allCoords.contains)) {
      val fst = fstCoords.map(v(_)(_)).mkString("")
      val snd = sndCoords.map(v(_)(_)).mkString("")
      val isXMas = (fst == "MS" || fst == "SM") && (snd == "MS" || snd == "SM")
      if (isXMas) true
      else false
    } else false
  }

  def printD4(): Unit = {
    val puzzle = parsePuzzle("d4.txt")
    val coords = allCoords(puzzle)
    val xCs = xCoords(puzzle, coords).toList
    val aCs = aCoords(puzzle, coords)

    val d4t1 = xCs.map((x, y) => countXmas(puzzle, (x, y), coords)).sum
    val d4t2 = aCs.count((x, y) => isXmas(puzzle, (x, y), coords))

    println(d4t1)
    println(d4t2)
  }
}
