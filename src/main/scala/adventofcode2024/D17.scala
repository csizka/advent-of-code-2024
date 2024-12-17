package adventofcode2024
import java.lang.Math.*
import scala.io.Source
import scala.annotation.tailrec

object D17 {

  case class Status(a: Int, b: Int, c: Int, pointer: Int, prevOutputs: Vector[Int]) {
    def update(toUpdate: Char, newVal: Int): Status = toUpdate match {
      case 'a' => Status(newVal, b, c, pointer, prevOutputs)
      case 'b' => Status(a, newVal, c, pointer, prevOutputs)
      case 'c' => Status(a, b, newVal, pointer, prevOutputs)
      case 'p' => Status(a, b, c, newVal, prevOutputs)
    }

    def update(plusOutput: Vector[Int]): Status = {
      Status(a, b, c, pointer, prevOutputs ++ plusOutput)
    }

    def calcComboOp(opIx: Int): Int = opIx match {
      case num if num <= 3 => num
      case 4 => a
      case 5 => b
      case 6 => c
      case _ => -1
    }
  }

  def parseD17(path: String): (Status, Vector[Int]) = {
    def source = Source.fromResource(path)
    val digitRegex = "\\d+".r
    def inputs = source.getLines().toVector.flatMap { digitRegex.findAllIn(_).toVector.map(_.toInt)}
    source.close()

    inputs.match { case a +: b +: c +: inst => (Status(a,b,c, 0, Vector[Int]()), inst)}
  }

  sealed trait Operator {
    def apply(): Status
  }

  case class Operator0(comboOp: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val newA = status.a / pow(2, comboOp).toInt
      status.update('a', newA).update('p', status.pointer + 2)
    }
  }

  case class Operator1(op: Int, status: Status) extends Operator  {
    override def apply(): Status = {
      val newB = status.b^op
      status.update('b', newB).update('p', status.pointer + 2)
    }
  }

  case class Operator2(comboOp: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val newB = comboOp % 8
      status.update('b', newB).update('p', status.pointer + 2)
    }
  }

  case class Operator3(op: Int, status: Status) extends Operator {
    override def apply(): Status = {
      if (status.a == 0) status.update('p', status.pointer + 2)
      else {
        status.update('p', op)
      }
    }
  }

  case class Operator4(op: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val newB = status.b ^ status.c
      status.update('b', newB).update('p', status.pointer + 2)
    }
  }

  case class Operator5(comboOp: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val nexPrint = comboOp % 8
      status.update(Vector(nexPrint)).update('p', status.pointer + 2)
    }
  }

  case class Operator6(comboOp: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val newB = status.a / pow(2, comboOp).toInt
      status.update('b', newB).update('p', status.pointer + 2)
    }
  }

  case class Operator7(comboOp: Int, status: Status) extends Operator {
    override def apply(): Status = {
      val newC = status.a / pow(2, comboOp).toInt
      status.update('c', newC).update('p', status.pointer + 2)
    }
  }

  def parseOperator(operator: Int, operand: Option[Int], status: Status): Option[Operator] = (operator, operand) match {
    case (0, Some(op))  => Some(Operator0(status.calcComboOp(op), status))
    case (1, Some(op))  => Some(Operator1(op, status))
    case (2, Some(op))  => Some(Operator2(status.calcComboOp(op), status))
    case (3, Some(op))  => Some(Operator3(op, status))
    case (4, Some(op))  => Some(Operator4(op, status))
    case (5, Some(op))  => Some(Operator5(status.calcComboOp(op), status))
    case (6, Some(op))  => Some(Operator6(status.calcComboOp(op), status))
    case (7, Some(op))  => Some(Operator7(status.calcComboOp(op), status))
    case _              => None
  }

  @tailrec
  def doOperation(instructions: Vector[Int], status: Status): Vector[Int] = {
    if (status.pointer >= instructions.size || status.pointer < 0) status.prevOutputs
    else {
      println(status)
      val curOperatorInst = instructions(status.pointer)
      val curOperand = instructions.lift(status.pointer + 1)
      parseOperator(curOperatorInst, curOperand, status) match {
        case None => status.prevOutputs
        case Some(op) => doOperation(instructions, op.apply())
      }
    }
  }

  def d17(): Unit = {
    val (startStatus, instructions) = parseD17("test.txt")
    val d17t1 = doOperation(instructions, startStatus)

    println(d17t1)
  }

  def main(args: Array[String]): Unit = {
    d17()

  }
}
