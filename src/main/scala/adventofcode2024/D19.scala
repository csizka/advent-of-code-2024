package adventofcode2024

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.io.Source

object D19 {
  def parseD19(path: String): (Vector[String], Vector[String]) = {
    val source = Source.fromResource(path)
    val input = source.getLines().map(_.split(",").toVector).toVector
    source.close()
    (input.filter(_.size > 1).flatten.map(_.replaceAll(" ", "")), input.filter(_.size == 1).flatten.filter(_.nonEmpty) )
  }

  @tailrec
  def findOptions(restOrders: Set[String], options: Vector[String]): Boolean = {
    val nextRestOrders = restOrders.flatMap{ order =>
      val matches = options.filter(towel =>
        towel.equals(order.take(towel.length)))
      val newOrders = matches.map(towel => order.drop(towel.length))
      newOrders
    }
    val foundOrder = nextRestOrders.exists(_.isEmpty)
    if (foundOrder)
      true
    else if (nextRestOrders.isEmpty) false
    else findOptions(nextRestOrders, options)
  }

  def countOptions(
      curOrder: String,
      options: Vector[String],
      saved: scala.collection.mutable.Map[String, Long]
  ): Long = {
    if (saved.contains(curOrder)) saved(curOrder)
    else {
      val matches = options.filter(towel =>
        towel.equals(curOrder.take(towel.length)))
      val newOrders = matches.map(towel => curOrder.drop(towel.length))
      val count = newOrders.map(countOptions(_, options, saved)).sum
      saved += (curOrder -> count)
      count
    }
  }

  def d19T1(towels: Vector[String], patterns: Vector[String]): Int = {
    patterns.count(order => findOptions(Set(order), towels))
  }

  def d19T2(towels: Vector[String], patterns: Vector[String]): Long = {
    val startMap = scala.collection.mutable.Map(""->1L)
    patterns.map(order => countOptions(order, towels, startMap)).sum
  }

  def main(args: Array[String]): Unit = {
    val (towels, patterns) = parseD19("d19.txt")
    val d19t1 = d19T1(towels, patterns)
    val d19t2 = d19T2(towels, patterns)
//    println(d19t1)
    println(d19t2)
  }
}
