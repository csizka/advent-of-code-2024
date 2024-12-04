package adventofcode2024

import scala.io.Source

object D1to2 {
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

  //Day2 parser
  def parseInputLists(path: String): List[List[Int]] = {
    val lines: Iterator[String] = Source.fromResource(path).getLines
    lines.toList.map(_.split("""\s+""").map(_.toInt).toList)
  }

  //d2t1
  def lstIsSafe(lst: List[Int]): Boolean = lst match {
    case head :: rest =>
      val (lstIsSafe, dir, prevNum) = rest.foldLeft(true, 'n', head) {
        case ((true, 'n', prev), curHead) if prev <= curHead + 3 && prev >= curHead + 1 => (true, 'd', curHead)
        case ((true, 'n', prev), curHead) if prev + 1 <= curHead  && prev + 3 >= curHead  => (true, 'i', curHead)
        case ((true, 'd', prev), curHead) if prev <= curHead + 3 && prev >= curHead + 1 => (true, 'd', curHead)
        case ((true, 'i', prev), curHead)  if prev + 1 <= curHead  && prev + 3 >= curHead  => (true, 'i', curHead)
        case _ => (false, 'f', -1)
      }
      lstIsSafe
    case _ => false
  }

  def countSafeLsts(lsts: List[List[Int]], f: List[Int] => Boolean): Int = {
    lsts.count(f)
  }

  //d2t2
  def lstIsSafeModifiedV2(lst: List[Int]): Boolean = lst match {
    case head :: rest =>
      val (isSafe, dir, prevNum, passedCount) = rest.foldLeft(true, 'n', head, 0) {
        case ((true, 'n', prev, passed), curHead) if prev <= curHead + 3 && prev >= curHead + 1 => (true, 'd', curHead, passed + 1)
        case ((true, 'n', prev, passed), curHead) if prev + 1 <= curHead && prev + 3 >= curHead => (true, 'i', curHead, passed + 1)
        case ((true, 'd', prev, passed), curHead) if prev <= curHead + 3 && prev >= curHead + 1 => (true, 'd', curHead, passed + 1)
        case ((true, 'i', prev, passed), curHead) if prev + 1 <= curHead && prev + 3 >= curHead => (true, 'i', curHead, passed + 1)
        case ((_, _, _, passed), curHead)                                                       => (false, 'f', -1, passed)
      }

      if (isSafe) true
      else if (passedCount == 1)
        val dropFst = lst.drop(1)
        val dropSnd = lst.take(passedCount) ++ lst.drop(passedCount + 1)
        val dropThrd = lst.take(passedCount + 1) ++ lst.drop(passedCount + 2)
        lstIsSafe(dropFst) || lstIsSafe(dropSnd)
      else
        val dropFst = lst.take(passedCount) ++ lst.drop(passedCount + 1)
        val dropSnd = lst.take(passedCount + 1) ++ lst.drop(passedCount + 2)
        lstIsSafe(dropFst) || lstIsSafe(dropSnd)
    case _ => false
  }

  def printD2(): Unit = {
    val d2t1 = countSafeLsts(parseInputLists("d2t1.txt"), lstIsSafe)
    val d2t2 = countSafeLsts(parseInputLists("d2t1.txt"), lstIsSafeModifiedV2)

    println(d2t1)
    println(d2t2)
  }
}
