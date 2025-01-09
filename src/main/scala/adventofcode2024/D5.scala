package adventofcode2024

import scala.annotation.tailrec
import scala.io.Source

object D5 {
  val numRegex = "\\d{2}".r
  def parseRules(path: String): (Map[Int, Set[Int]], List[List[Int]]) = {
    val lines = Source.fromResource(path).getLines
    val separatedOrder = lines.map(_.split(",").toList).toList
    val orders = separatedOrder.filter(_.size > 1).map(_.map(_.toInt))

    val rules = separatedOrder.filter(_.size == 1).map(_.mkString(""))
    val map = rules.foldLeft(Map[Int, Set[Int]]()) { case (curMap, curStr) => numRegex.findAllIn(curStr).toList match {
        case lhs :: rhs :: Nil =>
          val fst = lhs.toInt
          val snd = rhs.toInt
          curMap + (fst -> (curMap.getOrElse(fst, Set()) + snd))
        case _ => curMap
      }
    }
    (map, orders)
  }

  //if the order is right the middle number is returned else 0
  def checkOrder(pages: List[Int], orderMap: Map[Int, Set[Int]]): Int = {
    val minddleIx = pages.size / 2 + 1
    val startOption: Option[Int] = None
    val (allPages, isCorrect, middleOption) = pages.foldLeft(Set[Int](), true, startOption) { case ((prevNums, canBeCorrect, middleOption), curNum) =>
      val sholdBeAfterCur = orderMap.getOrElse(curNum, Set())
      val curIsCoorect = sholdBeAfterCur.intersect(prevNums).isEmpty
      if (canBeCorrect && curIsCoorect && prevNums.size != pages.size / 2) (prevNums + curNum, canBeCorrect, middleOption)
      else if (canBeCorrect && curIsCoorect && prevNums.size == pages.size / 2) (prevNums + curNum, canBeCorrect, Some(curNum))
      else (Set(), false, None)
    }
    middleOption match {
      case Some(num) => num
      case None => 0
    }
  }

  def sumRightOrderMiddleNums(lst: List[List[Int]], orderMap: Map[Int, Set[Int]]): Int = {
    lst.map(checkOrder(_, orderMap)).sum
  }
  
  //correct the ordering and take the middle page num
  def reorderedMiddleNum(pages: List[Int], orderMap: Map[Int, Set[Int]]): Int = {
    val pagesSet = pages.toSet
    val relevantMap = orderMap.filter((key, value) => pagesSet.contains(key)).map((key, value) => key -> value.intersect(pagesSet))
    @tailrec
    def reorderHelper(oreredPages: Vector[Int], map: Map[Int, Set[Int]]): Int = {
      val toAdd = map.filter((key, value) => value.isEmpty).map((key, value) => key).toList
      val newMap = toAdd.foldLeft(map){ case (curMap, curKey) =>
      curMap.removed(curKey).map((key, value) => (key, value.filterNot(_ == curKey)))
      }
      val newOrderedPages = oreredPages ++ toAdd
      if (newMap.isEmpty) newOrderedPages(pages.size / 2)
      else reorderHelper(newOrderedPages, newMap)
    }
    reorderHelper(Vector[Int](), relevantMap)
  }
  
  def sumIncorrectlyOrderedMiddles(lst: List[List[Int]], orderMap: Map[Int, Set[Int]]): Int = {
    lst.map(reorderedMiddleNum(_, orderMap)).sum
  }

  def main(args: Array[String]): Unit = {
    val (orderMap, pagesInOrder) = parseRules("d5.txt")
    val d5t1 = sumRightOrderMiddleNums(pagesInOrder, orderMap)
    val incorrectOrders = pagesInOrder.filter(checkOrder(_, orderMap) == 0)
    val d5t2 = sumIncorrectlyOrderedMiddles(incorrectOrders, orderMap)
    assert(d5t1 == 5509)
    assert(d5t2 == 4407)
  }
}
