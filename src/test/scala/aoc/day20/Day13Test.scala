package aoc.day20

import hedgehog.munit.HedgehogSuite

import scala.annotation.tailrec
import scala.collection.mutable
class Day13Test extends HedgehogSuite {
  sealed trait Elem[+S] {}
  case class Value(value : Int) extends Elem[Int] {}
  case class EList(list : Seq[Elem[Int]]) extends Elem[Int]
  case class EValue(value : Int) extends Elem[Int]
  object Comma extends Elem[Int]
  object Open extends Elem[Int]


  val inputs =
    """[1,1,3,1,1]
      |[1,1,5,1,1]
      |
      |[[1],[2,3,4]]
      |[[1],4]
      |
      |[9]
      |[[8,7,6]]
      |
      |[[4,4],4,4]
      |[[4,4],4,4,4]
      |
      |[7,7,7,7]
      |[7,7,7]
      |
      |[]
      |[3]
      |
      |[[[]]]
      |[[]]
      |
      |[1,[2,[3,[4,[5,6,7]]]],8,9]
      |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin

  test("compare 1") {
    // Example usage
//    val s = "[[1,23,456],[2,3,4]]"
//    val s = "[[10,2,3],[2,3,4]]"
    val s = "[1,[2,[3,4],[5,6,7]],[8],9]"

    val parsed: Elem[Int] = parseElem(s)
    println(parsed)
  }

  def readNumber(s: String, i: Int): (Int, Int) = {
    var numString = ""
    var j = i
    while (j < s.length && s(j).isDigit) {
      numString += s(j)
      j += 1
    }
    (numString.toInt, j)
  }


  def parseElem(str: String): Elem[Int] = {
    str.foldLeft(mutable.Stack[Elem[Int]]()) { (stack, c) =>
      val st = c match {
        case '[' => stack.push(Open)
        case ',' => stack.push(Comma)
        case ']' => // pop until Open and push as EList
          val elist = takeToEList(stack)
          stack.push(elist)
        case number if number.isDigit =>
          stack.top match {
            case EValue(deciNum) =>
              stack.pop()
              stack.push(EValue(deciNum * 10 + number.toString.toInt))
            case _ => stack.push(EValue(number.toString.toInt))
          }
        case _ => stack // stack 에 top 이 number 라면 pop 해서 더하고, 아니라면 push number
      }
      st
    }
      .top
  }

  def takeToEList(stack: mutable.Stack[Elem[Int]]): Elem[Int] = {
    @tailrec
    def takeEList(elem: Elem[Int], list: List[Elem[Int]]): Elem[Int] = {
      elem match {
        case Open => EList(list)
        case EValue(_) => takeEList(stack.pop(), elem +: list )
        case EList(_) => takeEList(stack.pop(), elem +: list)
        case _ => takeEList(stack.pop(), list) // ignore elem
      }
    }

    takeEList(stack.pop(), List.empty)
  }

}