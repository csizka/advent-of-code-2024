package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object D11 {
  def parseD11(path: String): List[Long] = {
    val bufferedSource = Source.fromResource(path)
    val nums = bufferedSource.getLines().toList.flatMap(_.split(" ")map(_.toLong))
    bufferedSource.close()
    nums
  }

  @tailrec
  def d11T1(repetition: Int, stones: List[Long]): Long = {
    if (repetition > 0) {
      val newStones = stones.flatMap {num =>
        val length = Math.log10(num).toLong + 1
        if (num == 0) List(1L)
        else if (length % 2 == 0) List(num / Math.pow(10, length/ 2).toLong, num % Math.pow(10, length/ 2).toLong)
        else List(num * 2024L)
      }
        d11T1(repetition - 1, newStones)
    } else stones.sum
  }

  @tailrec
  def d11T2(repetition: Int, stones: Map[Long, Long]): Long = {
    if (repetition > 0) {
      val newStones = stones.foldLeft(Map[Long, Long]()) { case (curMap, (num, repetition)) =>
        val length = Math.log10(num).toLong + 1
        if (num == 0L) curMap + (1L -> (curMap.getOrElse(1L, 0L) + repetition))
        else if (length % 2 == 0) 
          val fstNum = num / Math.pow(10, length / 2).toLong
          val sndNum = num % Math.pow(10, length / 2).toLong
          val nextMap = curMap + 
            (fstNum -> (curMap.getOrElse(fstNum, 0L) + repetition))
          nextMap + (sndNum -> (nextMap.getOrElse(sndNum, 0L) + repetition))
        else curMap + (num * 2024 -> (curMap.getOrElse(num * 2024, 0L) + repetition))
      }
      d11T2(repetition - 1, newStones)
    } else stones.values.sum
  }

  def printD11(): Unit = {
    val stones = parseD11("d11.txt")
    val startMap = stones.groupBy(identity).map { case (x,y) => (x, y.size.toLong)}
    val d11t1 = d11T1(25, stones)
    val d11t2 = d11T2(75, startMap)
    println(d11t1)
    println(d11t2)
  }
}
