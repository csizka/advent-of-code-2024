package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

object D22 {
  def parseD22(path: String): Vector[Long] = {
    val source = Source.fromResource(path)
    val nums = source.getLines().toVector.map(_.toLong)
    source.close()
    nums
  }

  @tailrec
  def calcNthSecretNum(startNum: Long, n: Int): Long = {
    if (n == 0) startNum
    else
      val fstPartSecNum = (startNum ^ (startNum * 64)) % 16777216
      val sndPartSecNum = (fstPartSecNum / 32) ^ fstPartSecNum % 16777216
      val nextSecNum = ((sndPartSecNum * 2048) ^ sndPartSecNum) % 16777216
      calcNthSecretNum(nextSecNum, n - 1)
  }

  def d22T1(nums: Vector[Long]): Long = {
    nums.map(calcNthSecretNum(_, 2000)).sum
  }

  @tailrec
  def nBananas(startNum: Long, n: Int, changes: Vector[Int] = Vector()): Vector[Int] = {
    if (n == 0) changes
    else
      val nextNum = calcNthSecretNum(startNum, 1)
      nBananas(nextNum, n-1, changes :+ (nextNum % 10).toInt)
  }

  // TODO: with regular zip
  def getChanges(nums: Vector[Int]): Vector[Int] = {
    nums.zipWithIndex.drop(1).map { case (num, ix) => num - nums(ix-1)}
  }

  def zipPatterns(nums: Vector[Int], changes: Vector[Int]): Vector[(Vector[Int], Int)] = {
    nums.zipWithIndex.drop(4).map{ case (num, ix) => (changes.slice(ix-4, ix), num) }
  }

  def getBanana(searchPool: Vector[(Vector[Int], Int)], pattern: Vector[Int]): Int = {
    val res = searchPool.find(_._1 == pattern).fold(0)(_._2)
    res
  }

  def findAllPatterns(searchPool: Vector[(Vector[Int], Int)]): Set[Vector[Int]] = {
    searchPool.toSet.map(_._1)
  }

  def d22T2(nums: Vector[Long]): Int = {
    val secrets = nums.map(nBananas(_, 2000))
    val searchPools = secrets.map{ nums =>
      val changes = getChanges(nums)
      zipPatterns(nums, changes)
    }
    val allPatterns = searchPools.toSet.flatMap(findAllPatterns)
    allPatterns.toVector.zipWithIndex.par.map{ case (pattern, ix) =>
      if (ix % 1000 == 0) println(ix)
      searchPools.map(getBanana(_, pattern)).sum
    }.max
  }

  def main(args: Array[String]): Unit = {
    val nums = parseD22("d22.txt")
//    val d22t1 = d22T1(nums)
    val d22t2 = d22T2(nums)
//    assert(d22t1 == 37327623)
    println(d22t2)
  }

}
