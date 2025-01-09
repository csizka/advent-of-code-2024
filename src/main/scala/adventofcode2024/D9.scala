package adventofcode2024

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

object D9 {
  def parseD9(path: String): (Vector[(Int, Int)], Vector[(Int, Int)]) = {
    val input = Source.fromResource(path).getLines().toVector.flatMap(_.toCharArray.map(_.asDigit)).zipWithIndex
    val files = input.collect { case (x,ix) if ix % 2 == 0 => (x, ix / 2)}
    val gaps = input.collect { case (x,ix) if ix % 2 == 1 => (x, ix / 2)}
    (files, gaps)
  }

  def d9T1(parsedFiles: Vector[(Int, Int)], parsedGaps: Vector[(Int, Int)]): Long = {
    val reverseFiles = parsedFiles.reverse
    @tailrec
    def reorgHelper(
      acc: Queue[Int],
      files: Vector[(Int, Int)],
      revFiles: Vector[(Int, Int)],
      gaps: Vector[(Int, Int)]
      ): Queue[Int] = {
      val curFile = files.headOption
      val curRevFile = revFiles.headOption
      val curGap = gaps.headOption
      (curFile, curRevFile, curGap) match {
        case (Some(fileRep, fileNum), Some(revFileRep, revFileNum), Some(gapRep, gapNum)) 
          if fileNum <= gapNum && fileNum < revFileNum =>
            val newAcc = acc ++ Vector.fill(fileRep)(fileNum)
            reorgHelper(newAcc, files.tail, revFiles, gaps)
        case (Some(fileRep, fileNum), Some(revFileRep, revFileNum), Some(gapRep, gapNum))
          if fileNum > gapNum && gapNum < revFileNum && gapRep > revFileRep =>
            val newAcc = acc ++ Vector.fill(revFileRep)(revFileNum)
            reorgHelper(newAcc, files, revFiles.tail, (gapRep - revFileRep, gapNum) +: gaps.tail)
        case (Some(fileRep, fileNum), Some(revFileRep, revFileNum), Some(gapRep, gapNum))
          if fileNum > gapNum && gapNum < revFileNum && gapRep <= revFileRep =>
            val newAcc = acc ++ Vector.fill(gapRep)(revFileNum)
            reorgHelper(newAcc, files, (revFileRep - gapRep, revFileNum) +: revFiles.tail, gaps.tail)
        case (Some(fileRep, fileNum), Some(revFileRep, revFileNum), None) 
          if fileNum < revFileNum =>
            val newAcc = acc ++ Vector.fill(fileRep)(fileNum)
            reorgHelper(newAcc, files.tail, revFiles, gaps)
        case (Some(fileRep, fileNum), Some(revFileRep, revFileNum), None) 
          if fileNum == revFileNum =>
            acc ++ List.fill(revFileRep)(revFileNum)  
        case _ => acc
      }
    }

    val reordedFiles = reorgHelper(Queue[Int](), parsedFiles, reverseFiles, parsedGaps).toList
    reordedFiles.zipWithIndex.map((num, ix) => num * ix.toLong).sum
  }

  def d9T2(parsedFiles: Vector[(Int, Int)], parsedGaps: Vector[(Int, Int)]): Long = {
    val startVector = (1 to parsedFiles.size).toVector.map(_ => Queue[(Int, Int)]())
    val (finalFiles, restGaps) = parsedFiles.foldRight(startVector, parsedGaps) { case ((curRep, curNum), (curVector, curGaps)) =>
      curGaps.find((rep, num) => rep >= curRep && num < curNum) match {
        case Some(rep, num) =>
          val newVector = curVector.updated(num, curVector(num).appended((curRep, curNum)))
          val newGaps =
            val gaps = curGaps.updated(num, (rep - curRep, num))
            gaps.updated(curNum - 1, (gaps(curNum - 1)._1 + curRep, gaps(curNum - 1)._2))
          (newVector, newGaps)
        case _ =>
          (curVector.updated(curNum, curVector(curNum).prepended((curRep, curNum))), curGaps)
      }
    }
    val finalGaps = restGaps.map((rep, num)=> List.fill(rep)(0))
    val finalList = finalFiles.map(_.flatMap((x,y) => List.fill(x)(y))).map(_.toList)
    val merged = finalList.zip(finalGaps).flatMap{ case (lhs, rhs) => lhs ++ rhs}
    merged.zipWithIndex.map((num, ix) => num * ix.toLong).sum
  }

  def main(args: Array[String]): Unit = {
    val (parsedFiles, parsedGaps) = parseD9("d9.txt")
    val d9t1 = d9T1(parsedFiles, parsedGaps)
    val d9t2 = d9T2(parsedFiles, parsedGaps)
    assert(d9t1 == 6279058075753L)
    assert(d9t2 == 6301361958738L)
  }
}