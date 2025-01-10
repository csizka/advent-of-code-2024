package adventofcode2024
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import guru.nidi.graphviz.attribute.{Font, Rank}
import guru.nidi.graphviz.engine.{Format, Graphviz}
import guru.nidi.graphviz.model.{Node, Factory as gviz}
import guru.nidi.graphviz.attribute.Rank.RankDir.*
import guru.nidi.graphviz.model.Factory.{graph, node}

import java.io.File
import scala.util.Random

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

  def reverseDeps(gates: Map[String, InGate]): Map[String, Set[String]] = {
    gates.foldLeft(Map[String, Set[String]]()) { case (curOuts, out -> InGate(fst, snd, op)) =>
      curOuts.updatedWith(fst){
        case Some(set) => Some(set + out)
        case None => Some(Set(out))
      }.updatedWith(snd){
        case Some(set) => Some(set + out)
        case None => Some(Set(out))
      }
    }
  }

  def orderToEval(gates: Map[String, InGate], inputVars: Set[String]): Vector[Gate] = {
    val startQueue = Queue[String]().enqueueAll(inputVars)
    val outPuts = reverseDeps(gates)
    @tailrec
    def loop(curQueue: Queue[String], ordered: Vector[Gate], explored: Set[String]): Vector[Gate] = curQueue match {
      case curExplored +: rest if outPuts.contains(curExplored) =>
        val newlyExplored =
          outPuts(curExplored).filter{ curOut =>
            explored.contains(gates(curOut).lhsIn) &&
            explored.contains(gates(curOut).rhsIn) &&
            !explored.contains(curOut)
          }
        val gatesToAdd = newlyExplored.map { curVar =>
          val curIngate = gates(curVar)
          Gate(curIngate.lhsIn, curIngate.rhsIn, curVar, curIngate.operation)
        }
        val nextOrdered = ordered ++ gatesToAdd
        val nextExplored = explored ++ newlyExplored
        val nextQueue = rest.enqueueAll(newlyExplored)
        loop(nextQueue, nextOrdered, nextExplored)
      case curExplored +: rest => loop(rest, ordered, explored)
      case Queue() => ordered
    }

    loop(startQueue, Vector.empty, inputVars)
  }

  def calcDecimalForVars(vals: Map[String, Int], fstCharOfVars: Char): Long = {
    val relevantVals = vals.keySet.filter(_.charAt(0) == fstCharOfVars).toVector.sorted
    val res = relevantVals.zipWithIndex.map((valName, ix) => vals(valName) * Math.pow(2, ix).toLong).sum
    res
  }

  def getDeps(gates: Set[Gate]): Map[String, InGate] = {
    gates.map{ case Gate(fst, snd, out, op) => out -> InGate(fst, snd, op)}.toMap
  }

  def calcFinalVals(order: Vector[Gate], vals: Map[String, Int]): Map[String, Int] = {
    order.foldLeft(vals) { case (curVals, Gate(fstName, sndName, out, op)) =>
      val fst = curVals(fstName)
      val snd = curVals(sndName)
      op match {
        case Operation.OR => curVals + (out -> (fst | snd))
        case Operation.XOR => curVals + (out -> (fst ^ snd))
        case Operation.AND => curVals + (out -> (fst & snd))
      }
    }
  }

  def d24T1(gates: Set[Gate], vals: Map[String, Int]): Long = {
    val dependencies = getDeps(gates)
    val order = orderToEval(dependencies, vals.keySet)
    val finalVals = calcFinalVals(order, vals)
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
    inputVars: Set[String]
  ): (Map[String, InGate], Vector[Gate]) = {
    val fstDeps = deps(fstToSwap)
    val sndDeps = deps(sndToSwap)
    val newDeps = deps + (fstToSwap -> sndDeps) + (sndToSwap -> fstDeps)
    val newOrder = orderToEval(newDeps, inputVars)
    (newDeps, newOrder)
  }

  def d24T2(
    vals: Map[String, Int],
    dependencies: Map[String, InGate],
    swapped: Vector[String]
  ): Vector[String] = {
    val expRes = getExpectedRes(vals)
    val order = orderToEval(dependencies, vals.keySet)
    val finalVals = calcFinalVals(order, vals)
    val actualRes = calcDecimalForVars(finalVals, 'z')
    if (actualRes == expRes)
      swapped.sorted
    else Vector()
  }

  def addVarsToMap(char: String, toAdd: Long): Map[String, Int] = {
    val binary = toAdd.toBinaryString.reverse
    binary.zipWithIndex.map { case (value, ix) => (char + ix.toString.reverse.padTo(2, '0').reverse, value.asDigit) }.toMap
  }

  def findBadGateWithInputs(xs: Long, ys: Long, order: Vector[Gate]): Option[Int] = {
    val allXVals = addVarsToMap("x", xs)
    val allYVals = addVarsToMap("y", ys)
    val finalVals = calcFinalVals(order, allXVals ++ allYVals)
    val expectedZs = (xs + ys).toBinaryString.reverse
    val actualZs = calcDecimalForVars(finalVals, 'z').toBinaryString.reverse.padTo(46, '0')
    actualZs.indices.find( ix => expectedZs(ix) != actualZs(ix))
  }

  def findFstBadGate(dependencies: Map[String, InGate], order: Vector[Gate]): Option[Int] = {
    val rand = new Random(42)
    val start = Math.pow(2,44).toLong
    val end = Math.pow(2,45).toLong
    val minErrorIx =
      (1 to 50)
        .toVector
        .map(_ => start + rand.nextLong((end - start) + 1))
        .grouped(2)
        .flatMap { case Vector(fst, snd) => findBadGateWithInputs(fst, snd, order)}
        .minOption
    minErrorIx
  }

  def collectLinks(dependencies: Map[String, InGate]): Seq[guru.nidi.graphviz.model.LinkSource] = {
    dependencies.toSeq.flatMap { case outVar -> curIngate =>
      val nodeLabel = s"${curIngate.operation.toString.toUpperCase} -> $outVar"
      Seq(
        node(curIngate.lhsIn).link(node(outVar).`with`("label", nodeLabel)),
        node(curIngate.rhsIn).link(node(outVar).`with`("label", nodeLabel))
      )
    }
  }

  def visualizeGraph(dependencies: Map[String, InGate]): Unit = {
    val g = graph("graph")
      .directed
      .graphAttr.`with`(Rank.dir(LEFT_TO_RIGHT))
      .nodeAttr.`with`(Font.name("arial"))
      .linkAttr.`with`("class", "link-class")
      .`with`(collectLinks(dependencies) *)

    Graphviz.fromGraph(g).height(100).render(Format.SVG).toFile(new File("example/graph.svg"))
  }

  @tailrec
  def swaps(
    toSwap: Vector[String],
    deps:  Map[String, InGate],
    startVals:  Set[String],
    order: Vector[Gate]
  ): (Map[String, InGate], Vector[Gate]) = toSwap match {
    case fst +: snd +: rest =>
      val (newDeps, newOrder) = swapGates(deps, fst, snd, startVals)
      swaps(rest, newDeps, startVals, newOrder)
    case Vector() => (deps, order)
  }

  def main(args: Array[String]): Unit = {
    val (gates, startVals) = parseD24("d24.txt")
    val dependencies = getDeps(gates)
    val ogOrder = orderToEval(dependencies, startVals.keySet)
    val toSwap = Vector("z15", "fph", "z21", "gds", "wrk", "jrs", "cqk", "z34")
    val (newDeps, newOrder) = swaps(toSwap, dependencies, startVals.keySet, Vector[Gate]())
    val d24t1 = d24T1(gates, startVals)
    val d24t2 = d24T2(startVals, newDeps, toSwap)
    val fstBadGate = findFstBadGate(newDeps, newOrder)
    visualizeGraph(newDeps)
    val revDeps = reverseDeps(dependencies)

    assert(d24t1 == 46463754151024L)
    assert(d24t2 == Vector("cqk", "fph", "gds", "jrs", "wrk", "z15", "z21", "z34"))
  }
}
