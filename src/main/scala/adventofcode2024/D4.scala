package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D4 {
  def parseD4(path: String): Vector[Vector[Char]] = {
    val source = Source.fromResource(path)
    val puzzle = source.getLines.toVector.map(_.toVector)
    source.close()
    puzzle
  }

  val xmas = Vector('X', 'M', 'A', 'S')

  def specCoords(puzzle: Vector[Vector[Char]], char: Char): Vector[(Int, Int)] = {
    puzzle.indices.flatMap(x => puzzle(x).indices.map(y => (x, y))).toVector.filter(puzzle(_)(_) == char)
  }

  def isValidCoord(nextCoord: (Int, Int), maxX: Int, maxY: Int): Boolean = {
    val (nextX, nextY) = nextCoord
    nextX >= 0 && nextY >= 0 && nextX <= maxX && nextY <= maxY
  }

  def countXmasFromXCoords(puzzle: Vector[Vector[Char]], origin: (Int, Int)): Int = {
    val (xX, xY) = origin
    val maxX = puzzle.size - 1
    val maxY = puzzle(0).size - 1

    @tailrec
    def isXMas(coords: Vector[(Int, Int)]): Boolean = {
      val (curX, curY) +: (prevX, prevY) +: rest: Vector[(Int, Int)] = coords
      val (nextX, nextY) = (curX + (curX - prevX), curY + (curY - prevY))
      val letterIsCorrect = xmas(coords.size - 1) == puzzle(curX)(curY)
      val foundAllLetters = coords.size == 4
      val nextIsValidCoord = isValidCoord((nextX, nextY), maxX, maxY)
      if (letterIsCorrect && foundAllLetters) true
      else if (letterIsCorrect && nextIsValidCoord) isXMas((nextX, nextY) +: coords)
      else false
    }

    val nextCoords = (for {
      x <- xX - 1 to xX + 1 if x >= 0 && x <= maxX
      y <- xY - 1 to xY + 1 if y >= 0 && y <= maxY
    } yield (x, y)).toVector
    nextCoords.count((x, y) => isXMas(Vector((x, y), origin)))
  }

  def isXmas(puzzle: Vector[Vector[Char]], origin: (Int, Int)): Boolean = {
    val maxX = puzzle.size - 1
    val maxY = puzzle(0).size - 1
    val (xX, xY) = origin
    val fstCoords = List((xX - 1, xY - 1), (xX + 1, xY + 1))
    val sndCoords = List((xX - 1, xY + 1), (xX + 1, xY - 1))
    if (fstCoords.forall(isValidCoord(_, maxX, maxY)) && sndCoords.forall(isValidCoord(_, maxX, maxY))) {
      val fst = fstCoords.map(puzzle(_)(_)).mkString("")
      val snd = sndCoords.map(puzzle(_)(_)).mkString("")
      val isXMas = (fst == "MS" || fst == "SM") && (snd == "MS" || snd == "SM")
      isXMas
    } else false
  }

  def d4(): (Int, Int) = {
    val puzzle = parseD4("d4.txt")
    val xCoords = specCoords(puzzle, 'X')
    val aCoords = specCoords(puzzle, 'A')

    val d4t1 = xCoords.map((x, y) => countXmasFromXCoords(puzzle, (x, y))).sum
    val d4t2 = aCoords.count((x, y) => isXmas(puzzle, (x, y)))

    (d4t1,d4t2)
  }
}
