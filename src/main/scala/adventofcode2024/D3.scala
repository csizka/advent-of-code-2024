package adventofcode2024

import scala.io.Source
import scala.util.matching.Regex

object D3 {

  def parseD3(path: String): String = {
    val bufferedSource = Source.fromResource(path)
    val string = bufferedSource.getLines.mkString("")
    bufferedSource.close()
    string
  }

  val t1SearchedValue = "mul\\(\\d{1,3},\\d{1,3}\\)".r

  def productSum(lst: Vector[(Int, Int)]): Int = {
    lst.map((fst, snd) => fst * snd).sum
  }
  
  def d3T1(input: String): Int = {
    val selectedStrings =
      t1SearchedValue.
        findAllIn(input)
        .toVector
        .map(_.split(",").map("\\d+".r.findFirstIn(_)))
        .map { case Array(Some(fst), Some(snd)) => (fst.toInt, snd.toInt) }
    productSum(selectedStrings)
  }

  val t2SeachedValue = "mul\\(\\d{1,3},\\d{1,3}\\)|do(n't)?\\(\\)".r

  def d3T2(input: String): Int = {
    val selectedStrings = t2SeachedValue.findAllIn(input).toVector
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

  def d3(): (Int, Int) = {
    val parsedInput = parseD3("d3t1.txt")
    val d3t1 = d3T1(parsedInput)
    val d3t2 = d3T2(parsedInput)

    (d3t1,d3t2)
  }  
}
