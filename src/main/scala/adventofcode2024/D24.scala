package adventofcode2024
import scala.annotation.tailrec
import scala.collection.immutable.Queue
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

  case class InGate (
    lhsIn: String,
    rhsIn: String,
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

  def orderToEval(gates: Map[String, InGate], inputVars: Set[String], outPuts: Map[String, Set[String]]): Vector[Gate] = {
    val startQueue = Queue[String]().enqueueAll(inputVars)
    @tailrec
    def loop(curQueue: Queue[String], ordered: Vector[Gate], explored: Set[String]): Vector[Gate] = curQueue match {
      case curExplored +: rest =>
        val newlyExplored =
          outPuts(curExplored).filter{ curOut => explored.contains(gates(curOut).lhsIn) && explored.contains(gates(curOut).rhsIn)}
        val gatesToAdd = newlyExplored.map { curVar =>
          val curIngate = gates(curVar)
          Gate(curIngate.lhsIn, curIngate.rhsIn, curVar, curIngate.operation)
        }
        val nextOrdered = ordered ++ gatesToAdd
        val nextExplored = explored ++ newlyExplored
        val nextQueue = rest.enqueueAll(newlyExplored)
        loop(nextQueue, nextOrdered, nextExplored)
      case Queue() => ordered
    }

    loop(startQueue, Vector.empty, inputVars)
  }

  def calcDecimalForVars(vals: Map[String, Int], fstCharOfVars: Char): Long = {
    val relevantVals = vals.keySet.filter(_.charAt(0) == fstCharOfVars).toVector.sorted
    val res = relevantVals.zipWithIndex.map((valName, ix) => vals(valName) * Math.pow(2, ix).toLong).sum
    res
  }
  
  def getDepsAndIngates(gates: Set[Gate]): (Map[String, InGate], Map[String, Set[String]]) = {
    val (dependencies, outputs) =
      gates.foldLeft(Map[String, InGate](), Map[String, Set[String]]()) { case ((curDeps, curOuts), Gate(fst, snd, out, op)) =>
        (curDeps + (out -> InGate(fst, snd, op)),
          curOuts + (fst -> (curOuts.getOrElse(fst, Set()) + out)) + (snd -> (curOuts.getOrElse(snd, Set()) + out)))
      }
    (dependencies, outputs)
  }

  def d24T1(gates: Set[Gate], vals: Map[String, Int]): Long = {
    val (dependencies, outputs) = getDepsAndIngates(gates)
    val order = orderToEval(dependencies, vals.keySet, outputs)
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

  def swapGates(
    deps: Map[String, InGate],
    fstToSwap: String,
    sndToSwap: String,
    inputVars: Set[String],
    outputs: Map[String, Set[String]]
  ): Vector[Gate] = {
    val fstDeps = deps(fstToSwap)
    val sndDeps = deps(sndToSwap)
    val newDeps = deps + (fstToSwap -> sndDeps) + (sndToSwap -> fstDeps)
    val newOutputs = outputs +
      (fstDeps.lhsIn -> (outputs(fstDeps.lhsIn) - fstToSwap + sndToSwap)) +
      (fstDeps.rhsIn -> (outputs(fstDeps.rhsIn) - fstToSwap + sndToSwap)) +
      (sndDeps.lhsIn -> (outputs(sndDeps.lhsIn) - sndToSwap + fstToSwap)) +
      (sndDeps.rhsIn -> (outputs(sndDeps.rhsIn) - sndToSwap + fstToSwap))
    orderToEval(newDeps, inputVars, newOutputs)
  }

  def d24T2(gates: Set[Gate], vals: Map[String, Int], numToSwap: Int): Vector[String] = {
    val expRes = getExpectedRes(vals)
    val (dependencies, outputs) = getDepsAndIngates(gates)
    ???
  }

  def main(args: Array[String]): Unit = {
    val (gates, startVals) = parseD24("d24.txt")
//    val d24t1 = d24T1(gates, startVals)
//    println(d24t1)
  }
}
