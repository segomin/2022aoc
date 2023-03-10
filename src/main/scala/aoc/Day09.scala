package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day09 extends App {
  val filename = "src/main/scala/aoc/Day09.input"
  val lines = Source.fromFile(filename).getLines()
  val river = River.of(lines.toArray)
  val part1 = river.countTailVisit()

  println(part1)
}


class River(val river: mutable.HashSet[Knot], var head: Knot, var tail: Knot) {
  def moveAll(inputs: Array[String]) = {
    inputs.foreach(line => {
      val console = line.split(" ")
      console match {
        case Array(direction, step) => moveHead(direction, step.toInt)
      }
    })
    this
  }

  def countTailVisit() = river.size

  def moveHead(direction: String, step: Int): Unit = {
    head = direction match {
      case "R" => head.rightTo(step)
      case "L" => head.leftTo(step)
      case "U" => head.upTo(step)
      case "D" => head.downTo(step)
    };
    tail.followAndMark(head, river)
    tail.followAndMark(head, river)
    tail.followAndMark(head, river)
    tail.followAndMark(head, river)
  }
}

object River {
  def of(inputs: Array[String]): River = {
    val tailBoard = new mutable.HashSet[Knot]()
    val tail = Knot(0, 0)
    val head = Knot(0, 0)
    new River(tailBoard, head, tail).moveAll(inputs)
  }
}

case class Knot(x: Int, y: Int) {
  def moveToward(head: Knot): Knot = {
    if (head.x == x) {
      Knot(x, if (head.y > y) y + 1 else y - 1)
    } else if (head.y == y) {
      Knot(if (head.x > x) x + 1 else x - 1, y)
    } else throw new RuntimeException("")
  }

  def moveToLine(head: Knot): Knot = {
    if (Math.abs(head.x - x) == 1) {
      Knot(head.x, y)
    } else if (Math.abs(head.y - y) == 1){
      Knot(x, head.y)
    } else throw new RuntimeException("")
  }

  def notInLine(head: Knot): Boolean = head.x != x && head.y != y

  def isNear(head: Knot): Boolean = {
    if (Math.abs(x - head.x) < 2 && Math.abs(y - head.y) < 2) true
    else false
  }

  def followAndMark(head: Knot, board: mutable.HashSet[Knot]): Knot = {

    @tailrec
    def followAndMark(head: Knot, tail: Knot): Knot = {
      board.add(tail)

      if (tail.isNear(head)) {
        return tail
      }

      var tail2 = tail
      if (tail.notInLine(head))
        tail2 = tail.moveToLine(head)

      followAndMark(head, tail2.moveToward(head))
    }

    followAndMark(head, this)
  }

  def rightTo(step: Int): Knot = Knot(x + step, y)
  def leftTo(step: Int): Knot = Knot(x - step, y)
  def upTo(step: Int): Knot = Knot(x, y + step)
  def downTo(step: Int): Knot = Knot(x, y - step)
}
