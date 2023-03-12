package aoc.day10

import scala.collection.mutable
import scala.io.Source

object Day06 extends App {
  private val expectLength = 14

  val filename = "src/main/scala/aoc/Day06.input"
  val line = Source.fromFile(filename).getLines().toList(0)
  def findUniqueIndex(line: Array[Char], queue: mutable.Queue[Char], index: Int): Int = {
    if (queue.size == expectLength) {
      return index
    }

    if (queue.contains(line(0))) {
      queue.removeHeadWhile(p => p != line(0))
      queue.remove(0)
    }
    queue.addOne(line(0))

    findUniqueIndex(line.splitAt(1)._2, queue, index + 1)
  }

  val result = findUniqueIndex(line.toCharArray, new mutable.Queue[Char], 0)
  println(result)
}
