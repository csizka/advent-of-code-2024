package adventofcode2024

import scala.io.Source

object D7 {

  def parsed7(path: String): List[(Long, List[Long])] = {
    Source.fromResource(path)
        .getLines()
        .toList
        .map(_.split(":"))
        .map{ case Array(lhs, rhs) => (lhs.toLong, rhs.split("""\s+""").toList.filter(_.nonEmpty).map(_.toLong))}
  }

  def canEqualT1(total: Long, parts: List[Long], subtotal: Long): Boolean = parts match {
    case Nil => total == subtotal
    case head :: rest if subtotal > 0 && subtotal <= total => canEqualT1(total, rest, subtotal + head) || canEqualT1(total, rest, subtotal * head)
    case fst :: snd :: rest if subtotal == 0 => canEqualT1(total, rest, fst + snd) || canEqualT1(total, rest, fst * snd)
    case _ => false
  }

  def canEqualT2(total: Long, parts: List[Long], subtotal: Long): Boolean = parts match {
    case Nil => total == subtotal
    case head :: rest if subtotal > 0 && subtotal <= total => 
      canEqualT2(total, rest, subtotal + head) || 
        canEqualT2(total, rest, subtotal * head) ||
        canEqualT2(total, rest,(subtotal.toString + head).toLong)
    case fst :: snd :: rest if subtotal == 0 => 
      canEqualT2(total, rest, fst + snd) || 
        canEqualT2(total, rest, fst * snd) ||
        canEqualT2(total, rest, (fst.toString + snd).toLong)
    case _ => false
  }

  def main(args: Array[String]): Unit = {
    val parsedInput = parsed7("d7.txt")
    val d7t1 = parsedInput.filter{ case (total, parts) => canEqualT1(total, parts, 0)}.map(_._1).sum
    val d7t2 = parsedInput.filter{ case (total, parts) => canEqualT2(total, parts, 0)}.map(_._1).sum
    assert(d7t1 == 66343330034722L)
    assert(d7t2 == 637696070419031L)
  }

}
