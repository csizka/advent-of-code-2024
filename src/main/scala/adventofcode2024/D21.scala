package adventofcode2024
import scala.annotation.tailrec
import scala.io.Source

object D21 {
  def parseD21(path: String): Vector[Vector[Char]] = {
    val source = Source.fromResource(path)
    val codes = source.getLines().toVector.map(_.toCharArray.toVector)
    source.close()
    codes
  }

  case class Coord(x: Int, y: Int) {
    def +(otherCoord: Coord): Coord = {
      Coord(otherCoord.x + x, otherCoord.y + y)
    }

    def -(otherCoord: Coord): Coord = {
      Coord(x - otherCoord.x, y - otherCoord.y)
    }

    def distance(): Int = {
      Math.abs(x) + Math.abs(y)
    }
  }

  val coordFromChar = Map(
    '7'->Coord(0,0), '8'->Coord(0,1), '9'->Coord(0,2),
    '4'->Coord(1,0), '5'->Coord(1,1), '6'->Coord(1,2),
    '1'->Coord(2,0), '2'->Coord(2,1), '3'->Coord(2,2),
                     '0'->Coord(3,1), 'A'->Coord(3,2)
  )

  val finalPad = Map(
    Coord(0,0)->Some('7'), Coord(0,1)->Some('8'), Coord(0,2)->Some('9'),
    Coord(1,0)->Some('4'), Coord(1,1)->Some('5'), Coord(1,2)->Some('6'),
    Coord(2,0)->Some('1'), Coord(2,1)->Some('2'), Coord(2,2)->Some('3'),
    Coord(3,0)->None,      Coord(3,1)->Some('0'), Coord(3,2)->Some('A')
  )

  val up = Coord(-1,0)
  val down = Coord(1,0)
  val left = Coord(0,-1)
  val right = Coord(0,1)
  val push = Coord(0,0)
  val dirs = Set(up, down, left, right, push)

  val coordFromDir = Map(
                         up -> Coord(0, 1), push -> Coord(0, 2),
    left -> Coord(1, 0), down -> Coord(1, 1), right -> Coord(1, 2)
  )

  val robotPad = Map(
    Coord(0,0)->None, Coord(0,1)->Some(up), Coord(0,2)->Some(push),
    Coord(1,0)->Some(left), Coord(1,1)->Some(down), Coord(1,2)->Some(right)
  )

  @tailrec
  def finalPadMovesFromFstRobot(toPress: Vector[Char], curCoord: Coord = coordFromChar('A'), moves: Vector[Coord] = Vector()): Vector[Coord] = toPress match {
    case nextChar +: rest =>
      val destCoord = coordFromChar(nextChar)
      val toMove = destCoord - curCoord
      val distance = toMove.distance()
      val directions = dirs.filter(curDir => (toMove - curDir).distance() < distance)
      if (distance == 0) finalPadMovesFromFstRobot (toPress.drop (1), curCoord, moves :+ push)
      else moves.lastOption match {
        case Some(dir) if distance > (toMove - dir).distance() && finalPad(curCoord + dir).isDefined =>
          finalPadMovesFromFstRobot(toPress, curCoord + dir, moves :+ dir)
        case _ =>
          val panicDir =
            if (curCoord.x == 3 && destCoord.y == 0) Some(left)
            else if (curCoord.y == 0 && destCoord.x == 3) Some(down) else None
          if (panicDir.isDefined && directions.size > 1) {
            val nonPanicDirs = directions - panicDir.get
            val nextDir = nonPanicDirs.head
            val nextCoord = nextDir + curCoord
            finalPadMovesFromFstRobot(toPress, nextCoord, moves :+ nextDir)
          } else {
            val nextDir = if (directions.contains(left)) left else if (directions.contains(down)) down else directions.head
            val nextCoord = nextDir + curCoord
            finalPadMovesFromFstRobot(toPress, nextCoord, moves :+ nextDir)
          }
      }
    case Vector() => moves
  }

  @tailrec
  def movesFromRobotToRobot(toPress: Vector[Coord], curCoord: Coord = coordFromDir(push), moves: Vector[Coord] = Vector()): Vector[Coord] = toPress match {
    case nextDir +: rest =>
      val destCoord = coordFromDir(nextDir)
      val toMove = destCoord - curCoord
      val distance = toMove.distance()
      val directions = dirs.filter(curDir => (toMove - curDir).distance() < distance)
      if (distance == 0) movesFromRobotToRobot(toPress.drop(1), curCoord, moves :+ push)
      else moves.lastOption match {
        case Some(dir) if distance > (toMove - dir).distance() && robotPad(curCoord + dir).isDefined =>
          movesFromRobotToRobot(toPress, curCoord + dir, moves :+ dir)
        case _ =>
          val panicDir =
            if (curCoord.x == 0 && destCoord == Coord(1,0)) Some(left)
            else if (curCoord == Coord(1,0) && destCoord.x == 0) Some(up) else None
          if (panicDir.isDefined && directions.size > 1) {
            val nonPanicDirs = directions - panicDir.get
            val nextDir = nonPanicDirs.head
            val nextCoord = nextDir + curCoord
            movesFromRobotToRobot(toPress, nextCoord, moves :+ nextDir)
          } else {
            val nextDir = if (directions.contains(left)) left else if (directions.contains(down)) down else directions.head
            val nextCoord = nextDir + curCoord
            movesFromRobotToRobot(toPress, nextCoord, moves :+ nextDir)
          }
      }
    case Vector() => moves
  }

  @tailrec
  def callRobotToRobotNTimes(prevInput: Vector[Coord], repetitions: Int): Vector[Coord] = {
    println(repetitions)
    if (repetitions == 1) movesFromRobotToRobot(prevInput)
    else callRobotToRobotNTimes(movesFromRobotToRobot(prevInput), repetitions - 1)
  }

  def digitRegex = "\\d+".r

  def getNum(input: Vector[Char]): Int = {
    digitRegex.findFirstIn(input.mkString("")).get.toInt
  }

  def d21T1(inputs: Vector[Vector[Char]]): Int = {
    inputs.map(curChars => getNum(curChars) * callRobotToRobotNTimes(finalPadMovesFromFstRobot(curChars), 2).size).sum
  }

  def d21T2(inputs: Vector[Vector[Char]]): Int = {
    inputs.map(curChars => getNum(curChars) * callRobotToRobotNTimes(finalPadMovesFromFstRobot(curChars), 25).size).sum
  }


  def main(args: Array[String]): Unit = {
    val inputs = parseD21("d21.txt")
//    val d21t1 = d21T1(inputs)
//    val d21t2 = d21T2(inputs)
//    println(d21t1)
//    println(d21t2)

  }
}
