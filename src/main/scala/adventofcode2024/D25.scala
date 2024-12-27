package adventofcode2024
import scala.io.Source

object D25 {
  def parseD25(path: String): (Vector[Vector[Int]], Vector[Vector[Int]]) = {
    val source = Source.fromResource(path)
    val (keys, locks) = 
      source
        .getLines()
        .toVector
        .grouped(8)
        .toVector
        .map(_.dropRight(1).map(_.toCharArray.toVector))
        .foldLeft(Vector[Vector[Int]](), Vector[Vector[Int]]()) { case ((curKeys, curLocks), toDo) => toDo(0)(0) match {
          case '#' => 
            val heights = 
              toDo(0)
                .indices
                .map(colIx => toDo.indices.count(rowIx => toDo(rowIx)(colIx) == '#') - 1).toVector
            (curKeys :+ heights, curLocks)
          case _ =>
            val heights =
              toDo(0)
                .indices
                .map(colIx => toDo.indices.count(rowIx => toDo(rowIx)(colIx) == '.') - 1).toVector
            (curKeys, curLocks :+ heights)
        }}
    source.close()
    (keys, locks)
  }
  
  def countFittingKeys(lock: Vector[Int], keys: Vector[Vector[Int]]): Int = {
    keys.count(key => key.indices.forall(ix => key(ix) >= lock(ix)))
  }
  
  def d25T1(locks: Vector[Vector[Int]], keys: Vector[Vector[Int]]): Int = {
    locks.map(countFittingKeys(_, keys)).sum
  }
  
  def main(args: Array[String]): Unit = {
    val (keys, locks) = parseD25("d25.txt")
    val d25t1 = d25T1(keys, locks)
    println(d25t1)
  }
}
