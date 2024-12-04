package adventofcode2024

import scala.io.Source
import scala.util.matching.Regex

object D3to4 {

  val mulMatchPattern: Regex = "mul\\(\\d{1,3},\\d{1,3}\\)".r

  def parseInputString(path: String): List[(Int, Int)] = {
    val lines: Iterator[String] = Source.fromResource(path).getLines
    val string = lines.mkString("")
    val selectedStrings =
      mulMatchPattern.
        findAllIn(string)
        .toList
        .map(_.split(",").map("\\d+".r.findFirstIn(_)))
        .map{ case Array(Some(fst), Some(snd)) => (fst.toInt, snd.toInt)}
    selectedStrings
  }

  def productSum(lst: List[(Int, Int)]): Int = {
    lst.map( (fst, snd) => fst * snd).sum
  }

  val mulMatchPatternV2: Regex = "mul\\(\\d{1,3},\\d{1,3}\\)|do(n't)?\\(\\)".r

  def day3task2(path: String): Int = {
    val lines: Iterator[String] = Source.fromResource(path).getLines
    val string = lines.mkString("")
    val selectedStrings = mulMatchPatternV2.findAllIn(string).toList
    val (res, lastState) = selectedStrings.foldLeft(0, true) { 
      case ((curRes, shouldBeAdded), curStr) => curStr match {
        case "do()" => (curRes, true)
        case "don't()" => (curRes, false)
        case str if shouldBeAdded => 
          val (lhs, rhs) = str.split(",").map("\\d+".r.findFirstIn(_)) match {
              case Array(Some(fst), Some(snd)) => (fst.toInt, snd.toInt)
            }
          (curRes + (lhs * rhs), true)
        case _ => (curRes, shouldBeAdded)
      }}
    res 
  }
  
  def printD3(): Unit = {
    val d3t1 = productSum(parseInputString("d3t1.txt"))
    val d3t2 = day3task2("d3t1.txt")

    println(d3t1)
    println(d3t2)
  }


}
