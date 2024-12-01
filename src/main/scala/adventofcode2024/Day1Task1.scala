package adventofcode2024

import scala.io.Source

object Day1Task1 {
  def parseInput(path: String): (List[Int], List[Int]) = {
    val lines: Iterator[String] = Source.fromResource(path).getLines
    val splitNums = lines.toList.map(_.split("""\s+""").toList)
    
    val (lhsList, rhsList) = splitNums.map{ case x :: y :: _ => (x.toInt,y.toInt)}.unzip

    (lhsList.sorted, rhsList.sorted)
  }
  def sumDiff(lists: (List[Int], List[Int])): Int = {
    val (lhs, rhs) = lists
    lhs.zip(rhs).map((x, y) => Math.abs(x-y)).sum
  }
}
