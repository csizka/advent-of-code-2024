package adventofcode2024

import scala.io.Source

object D2 {

  def parseD2(path: String): Vector[Vector[Int]] = {
    val bufferedSource = Source.fromResource(path)
    val reports = bufferedSource.getLines.toVector.map(_.split("""\s+""").map(_.toInt).toVector)
    bufferedSource.close()
    reports
  }

  def isCoorectIncrease(fst: Int, snd: Int): Boolean = {
    fst + 1 <= snd && fst + 3 >= snd
  }

  def isCoorectDecrease(fst: Int, snd: Int): Boolean = {
    snd + 1 <= fst && snd + 3 >= fst
  }

  def levelsAreSafe(levels: Vector[Int]): Boolean = levels match {
    case head +: rest =>
      val (levelsAreSafe, changeDir, prevNum) = rest.foldLeft(true, 'n', head) {
        case ((true, 'n', prev), curHead) if isCoorectDecrease(prev, curHead) => (true, 'd', curHead)
        case ((true, 'n', prev), curHead) if isCoorectIncrease(prev, curHead) => (true, 'i', curHead)
        case ((true, 'd', prev), curHead) if isCoorectDecrease(prev, curHead) => (true, 'd', curHead)
        case ((true, 'i', prev), curHead) if isCoorectIncrease(prev, curHead) => (true, 'i', curHead)
        case _ => (false, 'f', -1)
      }
      levelsAreSafe
    case _ => false
  }

  def levelsAreSafeModified(lst: Vector[Int]): Boolean = lst match {
    case head +: rest =>
      val (isSafe, dir, prevNum, passedCount) = rest.foldLeft(true, 'n', head, 0) {
        case ((true, 'n', prev, passed), curHead) if isCoorectDecrease(prev, curHead) => (true, 'd', curHead, passed + 1)
        case ((true, 'n', prev, passed), curHead) if isCoorectIncrease(prev, curHead) => (true, 'i', curHead, passed + 1)
        case ((true, 'd', prev, passed), curHead) if isCoorectDecrease(prev, curHead) => (true, 'd', curHead, passed + 1)
        case ((true, 'i', prev, passed), curHead) if isCoorectIncrease(prev, curHead) => (true, 'i', curHead, passed + 1)
        case ((_, _, _, passed), curHead) => (false, 'f', -1, passed)
      }

      if (isSafe) true
      else if (passedCount == 1){
        val dropFst = lst.drop(1)
        val dropSnd = lst.take(passedCount) ++ lst.drop(passedCount + 1)
        val dropThrd = lst.take(passedCount + 1) ++ lst.drop(passedCount + 2)
        levelsAreSafe(dropFst) || levelsAreSafe(dropSnd)
      } else {
        val dropFst = lst.take(passedCount) ++ lst.drop(passedCount + 1)
        val dropSnd = lst.take(passedCount + 1) ++ lst.drop(passedCount + 2)
        levelsAreSafe(dropFst) || levelsAreSafe(dropSnd)
      }
    case _ => false
  }

  def countSafeReports(reports: Vector[Vector[Int]], filter: Vector[Int] => Boolean): Int = {
    reports.count(filter)
  }

  def d2(): (Int, Int) = {
    val parsedReports = parseD2("d2.txt")
    val d2t1 = countSafeReports(parsedReports, levelsAreSafe)
    val d2t2 = countSafeReports(parsedReports, levelsAreSafeModified)
    (d2t1,d2t2)
  }
}
