package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D13 {

  val numRegex = """\d+""".r
  def parseD13(path: String): Vector[((Long, Long), (Long, Long), (Long, Long))] = {
    val source = Source.fromResource(path)
    val stats =
      source
        .getLines()
        .toVector
        .flatMap(_.split(","))
        .map(numRegex.findFirstIn(_))
        .flatMap{case Some(num) => Vector(num.toLong) case _ => Vector()}
        .grouped(6)
        .toVector
        .map{ case Vector(aX, aY, bX, bY, targetX, targetY) => ((aX, aY), (bX, bY), (targetX, targetY))}
    source.close()
    stats
  }

  def betterChoiceIX(stats:((Long, Long), (Long, Long), (Long, Long))): Int = {
    val ((aX, aY), (bX, bY), (targetX, targetY)) = stats
    val (correctedBX, correctedBY) = (bX * 3, bY * 3)
    val aFactor = Math.abs(targetX/aX.toDouble - targetY/aY.toDouble)
    val bFactor = Math.abs(targetX/correctedBX - targetY/correctedBY)
    if (aFactor < bFactor) 0
    else if (aFactor > bFactor) 1
    else if (aX + aY > bX + bY ) 0
    else 1
  }

  def countTokens(stats:((Long, Long), (Long, Long), (Long, Long))): Option[Long] = {
    val ((aX, aY), (bX, bY), (targetX, targetY)) = stats
    val betterIx = betterChoiceIX(stats)
    val (betterX, betterY) = if (betterIx == 0 ) (aX, aY) else (bX, bY)
    val (worseX, worseY) = if (betterIx == 1 ) (aX, aY) else (bX, bY)

    @tailrec
    def calcCost(remX: Long, remY: Long, usedWorse: Long): Option[(Long, Long)] = {
      val betterXDividant = remX / betterX
      val betterYDividant = remY / betterY
      if ( betterXDividant == betterYDividant && remX % betterX == 0 && remY % betterY == 0) Some(usedWorse, betterXDividant)
      else if (remX >= worseX && remY >= worseY) calcCost(remX - worseX, remY - worseY, usedWorse + 1)
      else None
    }
    calcCost(targetX, targetY, 0) match {
      case Some((worseCount, betterCount)) if betterIx == 0 => Some(worseCount + betterCount * 3)
      case Some((worseCount, betterCount)) if betterIx == 1 => Some(worseCount * 3 + betterCount)
      case _ => None
    }
  }

  def countTokensV2(stats: ((Long, Long), (Long, Long), (Long, Long))): Option[Long] = {
    val ((aX, aY), (bX, bY), (targetX, targetY)) = stats
    val n = (targetY * bX - bY * targetX) / (bX * aY - bY * aX)
    val m = (targetX - n * aX) / bX
    if (targetX == n * aX + m * bX && targetY == n * aY + m * bY) Some(n * 3 + m)
    else None
  }

  def d13(): (Long, Long) = {
    val input = parseD13("d13.txt")
    val modInput = input.map{ case ((aX, aY), (bX, bY), (targetX, targetY)) => ((aX, aY), (bX, bY), (targetX + 10000000000000L, targetY + 10000000000000L))}
    val d13t1 = input.flatMap(countTokens).sum
    val d13t2 = modInput.flatMap(countTokensV2).sum

    (d13t1, d13t2)
  }
}
