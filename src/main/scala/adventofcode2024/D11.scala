package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D11 {
  def parseD11(path: String): List[Long] = {
    val bufferedSource = Source.fromResource(path)
    val nums = bufferedSource.getLines().toList.flatMap(_.split(" ")map(_.toLong))
    bufferedSource.close()
    nums
  }
  
  def splitNum(num: Long, length: Long): List[Long] = {
    val divisor = Math.pow(10, length / 2).toLong
    val fstPart = num / divisor
    val sndPart = num % divisor
    List(fstPart, sndPart)
  }
  
  def calcNextStones(num: Long): List[Long] = {
    val length = Math.log10(num).toLong + 1
    if (num == 0) List(1)
    else if (length % 2 == 0) {
      splitNum(num, length)
    }
    else List(num * 2024)
  }

  @tailrec
  def d11T1(repetition: Int, stones: List[Long]): Long = {
    if (repetition > 0) {
      val newStones = stones.flatMap(calcNextStones)
      d11T1(repetition - 1, newStones)
    } else stones.size
  }

  @tailrec
  def d11T2(repetition: Int, stones: Map[Long, Long]): Long = {
    if (repetition > 0) {
      val newStones = stones.foldLeft(Map[Long, Long]()) { case (curMap, (num, repetition)) =>
        calcNextStones(num).foldLeft(curMap){ case (newMap, newNum) =>
          newMap + (newNum -> (newMap.getOrElse(newNum, 0L) + repetition))
        }
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
