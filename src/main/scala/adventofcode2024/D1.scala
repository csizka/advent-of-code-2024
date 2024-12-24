package adventofcode2024

import scala.io.Source

object D1 {

  def parseD1(path: String): (Vector[Int], Vector[Int]) = {
    val bufferedSource = Source.fromResource(path)
    val splitNums = bufferedSource.getLines.toVector.map(_.split("""\s+""").toList)
    bufferedSource.close
    val (lhsList, rhsList) = splitNums.map{ case x +: y +: _ => (x.toInt,y.toInt)}.unzip

    (lhsList.sorted, rhsList.sorted)
  }

  def sumDiff(lists: (Vector[Int], Vector[Int])): Int = {
    val (lhs, rhs) = lists
    lhs.zip(rhs).map((x, y) => Math.abs(x-y)).sum
  }

  def similarityScore(lsts: (Vector[Int], Vector[Int])): Int = {
    val (lhs, rhs) = lsts
    val lhsCount = lhs.groupBy(identity).map((num, lst) => (num, lst.size))
    val rhsCount = rhs.groupBy(identity).map((num, lst) => (num, lst.size))
    lhsCount.map((num, count) => rhsCount.getOrElse(num, 0) * num * count).sum
  }

  def d1(): (Int, Int) = {
    val d1t1 = sumDiff(parseD1("d1.txt"))
    val d1t2 = similarityScore(parseD1("d1.txt"))
    (d1t1,d1t2)
  }
}
