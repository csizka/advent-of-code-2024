package adventofcode2024
import scala.annotation.tailrec
import scala.io.Source

object D23 {
  def parseD23(path: String): Vector[(String,String)] = {
    val source = Source.fromResource(path)
    val connections = source
      .getLines
      .toVector
      .map(_.split("-"))
      .map{ case Array(fst, snd) => (fst, snd)}
    source.close()
    connections
  }

  def d23T1(conns: Map[String, Set[String]]): Int = {
    val ts = conns.keySet.filter(_.take(1) == "t")
    val triangles = ts.flatMap(fst => conns(fst).flatMap(snd => conns(fst).intersect(conns(snd)).map(thrd => List(fst, snd, thrd).sorted)))
    triangles.size
  }

  def calcNextLevel(curNetwork: Set[String], conn: Map[String, Set[String]]): Set[Set[String]] = {
    val sharedNeighbours = curNetwork.map(conn(_)).reduce(_ intersect _)
    sharedNeighbours.map(curNetwork + _)
  }

  @tailrec
  def biggestNetwork(curLevel: Set[Set[String]], conn: Map[String, Set[String]]): Vector[String] = {
    if (curLevel.size == 1) curLevel.toVector.flatten.sorted
    else
      println(curLevel)
      val nextLevel = curLevel.flatMap( curLevelSet => calcNextLevel(curLevelSet, conn))
      biggestNetwork(nextLevel, conn)
  }

  def d23T2(pairs: Vector[(String,String)], conns: Map[String, Set[String]]): Vector[String] = {
    val initialNetworks = pairs.toSet.map((lhs, rhs) => Set(lhs, rhs))
    biggestNetwork(initialNetworks, conns)
  }

  def main(args: Array[String]): Unit = {
    val connections = parseD23("d23.txt")
    val conns = connections.foldLeft(Map[String, Set[String]]()) { case (curConns, (curLhs, curRhs)) =>
      val nextConns = curConns + (curLhs -> (curConns.getOrElse(curLhs, Set()) + curRhs))
      nextConns + (curRhs -> (curConns.getOrElse(curRhs, Set()) + curLhs))
    }
    val d23t1 = d23T1(conns)
    val d23t2 = d23T2(connections, conns)

    assert(d23t1 == 1077)
    assert(d23t2 == "bc, bf, do, dw, dx, ll, ol, qd, sc, ua, xc, yu, zt".split(", ").toVector)
  }
}
