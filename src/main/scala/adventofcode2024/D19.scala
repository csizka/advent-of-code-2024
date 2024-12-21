package adventofcode2024

import scala.annotation.tailrec
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

  @tailrec
  def findOptionsV2(restOrders: (Vector[String], Int), options: Vector[String]): Int = {
    val prevCount = restOrders._2
    val curOrders = restOrders._1.flatMap { order =>
      val matches = options.filter(towel =>
        towel.equals(order.take(towel.length)))
      val newOrders = matches.map(towel => order.drop(towel.length))
      newOrders
    }
    val nextRestOrders = curOrders.filter(_.nonEmpty)
    val nextCount = curOrders.count(_.isEmpty) + prevCount
    if (nextRestOrders.isEmpty) 
      println("count")
      nextCount
    else findOptionsV2((nextRestOrders, nextCount), options)
  }

  def d19T1(towels: Vector[String], patterns: Vector[String]): Int = {
    patterns.count(order => findOptions(Set(order), towels))
  }
  
  def d19T2(towels: Vector[String], patterns: Vector[String]): Int = {
    patterns.map(order => findOptionsV2((Vector(order), 0), towels)).sum
  }
  
  def main(args: Array[String]): Unit = {
    val (towels, patterns) = parseD19("d19.txt")
    val d19t1 = d19T1(towels, patterns)
    val d19t2 = d19T2(towels, patterns)
//    println(d19t1)
    println(d19t2)
  }
}
