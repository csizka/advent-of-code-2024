package adventofcode2024

import scala.io.Source

object D1 {
  //Day 1, parsing for both task1 and 2
  def parseInputTuples(path: String): (List[Int], List[Int]) = {
    val lines: Iterator[String] = Source.fromResource(path).getLines
    val splitNums = lines.toList.map(_.split("""\s+""").toList)

    val (lhsList, rhsList) = splitNums.map{ case x :: y :: _ => (x.toInt,y.toInt)}.unzip

    (lhsList.sorted, rhsList.sorted)
  }

  //task1
  def sumDiff(lists: (List[Int], List[Int])): Int = {
    val (lhs, rhs) = lists
    lhs.zip(rhs).map((x, y) => Math.abs(x-y)).sum
  }

  //task2
  def similarityScore(lsts: (List[Int], List[Int])): Int = {
    val (lhs, rhs) = lsts
    val lhsCount = lhs.groupBy(identity).map((num, lst) => (num, lst.size))
    val rhsCount = rhs.groupBy(identity).map((num, lst) => (num, lst.size))
    lhsCount.map((num, count) => rhsCount.getOrElse(num, 0) * num * count).sum
  }

  def printD1(): Unit = {
    val d1task1 = sumDiff(parseInputTuples("d1t1.txt"))
    val d1t2 = similarityScore(parseInputTuples("d1t1.txt"))
    println(s"$d1task1")
    println(s"$d1t2")
  }
}
