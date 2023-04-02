package aoc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day09 extends App {
  val filename = "src/main/scala/aoc/day10/Day09.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines.toArray

  val part1 = new River(2).moveAll(inputs).tailVisitCount()
  println(part1)

  val part2 = new River(10).moveAll(inputs).tailVisitCount()
  println(part2)
}


class River(ropeSize: Int) {

  var knots: Array[Knot] = Array.fill[Knot](ropeSize)(Knot(0, 0))
  val river: mutable.HashSet[Knot] = mutable.HashSet()
  def moveAll(inputs: Array[String]) = {
    inputs.foreach(line => {
      val console = line.split(" ")
      val (direction: String, step: Int) = (console(0), console(1).toInt)

      def moveTail(head: Knot, tail: Knot, mark: Knot => Unit): Knot = {
        @tailrec
        def followTail(head: Knot, tail: Knot): Knot = {
          mark(tail)

          if (tail.isNear(head)) {
            return tail
          }

          val moved = tail.moveToward(head)
          followTail(head, moved)
        }

        followTail(head, tail)
      }

      @tailrec
      def iterateMove(head: Knot, tails: Array[Knot], acc: Array[Knot]): Array[Knot] = {
        tails match {
          case Array(last) =>
            val moved = moveTail(head, last, knot => river.add(knot))
            iterateMove(moved, tails.tail, acc :+ moved);
          case Array(one, _*) =>
            val moved = moveTail(head, one, _ => ())
            iterateMove(moved, tails.tail, acc :+ moved);
          case _ => acc
        }
      }

      def moveSteps(direction: String): Unit = {
        val head = direction match {
          case "R" => knots.head.rightTo()
          case "L" => knots.head.leftTo()
          case "U" => knots.head.upTo()
          case "D" => knots.head.downTo()
        }
        val newKnots = iterateMove(head, knots.tail, Array(head))
        knots = newKnots
      }

      // part 1 에서 head 를 한번에 옮긴 후 tail 을 이동해 보았으나,
      // tail 이 길어질 경우 이렇게는 안되서 하나씩 옮기는 방식으로 변경함
      for(_ <- 0 until step) {
        moveSteps(direction)
      }
    })
    this
  }

  def tailVisitCount() = river.size
}

case class Knot(x: Int, y: Int) {
  def moveToward(target: Knot): Knot = {
    if (notInLine(target))
      return Knot(x + (target.x - x).sign, y + (target.y - y).sign)

    if (target.x == x) {
      Knot(x, y + (target.y - y).sign)
    } else if (target.y == y) {
      Knot(x + (target.x - x).sign, y)
    } else throw new RuntimeException("")
  }

  def notInLine(target: Knot): Boolean = target.x != x && target.y != y

  def isNear(target: Knot): Boolean = {
    if ((target.x - x).abs < 2 && (target.y - y).abs < 2) true
    else false
  }
  def rightTo(step: Int = 1): Knot = Knot(x + step, y)
  def leftTo(step: Int = 1): Knot = Knot(x - step, y)
  def upTo(step: Int = 1): Knot = Knot(x, y + step)
  def downTo(step: Int = 1): Knot = Knot(x, y - step)
}
