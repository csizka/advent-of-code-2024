package adventofcode2024
import scala.annotation.tailrec
import scala.io.Source

object D24 {
  enum Operation {
    case AND, XOR, OR
  }
  
  case class Gate (
    lhsIn: String,
    rhsIn: String,
    out: String,
    operation: Operation
  )
  
  def parseD24(path: String): (Set[Gate], Map[String, Int]) = {
    val source = Source.fromResource(path)
    val input = source.getLines().toSet
    val gates = input.collect{_.split(" ") match {
      case  Array(fstIn, op, sndIn, arrow, out) if op == "OR" => Gate(fstIn, sndIn, out, Operation.OR)
      case  Array(fstIn, op, sndIn, arrow, out) if op == "XOR" => Gate(fstIn, sndIn, out, Operation.XOR)
      case  Array(fstIn, op, sndIn, arrow, out) if op == "AND" => Gate(fstIn, sndIn, out, Operation.AND)
    }}
    val startVals = input.toVector.collect(_.split(": ") match {
      case Array(name, value) => (name, value.toInt)
    }).toMap
    source.close()
    (gates, startVals)
  }

  def orderToEval(gates: Set[Gate], inputVars: Set[String]) = {
    @tailrec
    def loop(gates: Set[Gate], computedVars: Set[String], ordered: Vector[Gate]): Vector[Gate] = {
      if (gates.nonEmpty) {
        val nextToSolve = gates.filter { case Gate(fst, snd, out, op) => computedVars.contains(fst) && computedVars.contains(snd) }
        val solvedVals = nextToSolve.map(_.out)
        loop(gates -- nextToSolve, computedVars ++ solvedVals, ordered ++ nextToSolve)
      } else ordered
    }

    loop(gates, inputVars, Vector.empty)
  }

  def calcDecimalForVars(vals: Map[String, Int], fstCharOfVars: Char): Long = {
    val relevantVals = vals.keySet.filter(_.charAt(0) == fstCharOfVars).toVector.sorted
    val res = relevantVals.zipWithIndex.map((valName, ix) => vals(valName) * Math.pow(2, ix).toLong).sum
    res
  }

  def d24T1(gates: Set[Gate], vals: Map[String, Int]): Long = {
    val order = orderToEval(gates, vals.keySet)
    val finalVals = order.foldLeft(vals){ case (curVals, Gate(fstName, sndName, out, op)) =>
      val fst = curVals(fstName)
      val snd = curVals(sndName)
      op match {
      case Operation.OR => curVals + (out -> (fst | snd))
      case Operation.XOR => curVals + (out -> (fst ^ snd))
      case Operation.AND => curVals + (out -> (fst & snd))
    }}
    val res = calcDecimalForVars(finalVals, 'z')
    res
  }

  def getExpectedRes(vals: Map[String, Int]): Long = {
    calcDecimalForVars(vals, 'x') + calcDecimalForVars(vals, 'y')
  }

  def resIsValid(vals: Map[String, Int], expected: Long): Boolean = {
    calcDecimalForVars(vals, 'z') == expected
  }

  @tailrec
  def collectAllsDeps(toExplore: Set[String], collected: Set[String], deps: Map[String, (String, String)]): Set[String] = {
    if (toExplore.isEmpty) collected
    else {
      val nextToExplore = toExplore.map(deps.get).flatMap{ case Some((fst,snd)) => Set(fst, snd)}
      collectAllsDeps(nextToExplore, collected ++ toExplore, deps)
    }
  }

  def possibleSwaps(gates: Set[Gate], toSwap: Gate, dependencies: Map[String, (String, String)]): Set[String] = {
    gates.map(_.out) -- collectAllsDeps(Set(toSwap.out), Set(), dependencies)
  }

//  def swapGates(allGates: Vector[Gate], deps: Map[String, (String, String)], fstToSwap: Gate, sndToSwap: Gate): Set[Gate] = {
//    val fstName = fstToSwap.out
//    val sndName = sndToSwap.out
//    val fstDeps = deps(fstName)
//    val sndDeps = deps(sndName)
//    val newDeps = deps +
//
//
//
//  }

  def d24T2(gates: Set[Gate], vals: Map[String, Int], numToSwap: Int): Vector[String] = {
    val validRes = getExpectedRes(vals)
    val dependencies = gates.foldLeft(Map[String, (String, String)]()) { case (curDeps, Gate(fst, snd, out, op)) =>
      curDeps + (out -> (fst, snd))
    }
    ???

  }

  def main(args: Array[String]): Unit = {
    val (gates, startVals) = parseD24("d24.txt")
    val d24t1 = d24T1(gates, startVals)
    println(d24t1)
  }
}
