package aoc.day20

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day12 extends App {

  val filename = "src/main/scala/aoc/day20/Day12.input"
  val lines = Source.fromFile(filename).getLines()
  val inputs = lines.mkString("\n")

  def findPoint(heightmap: Array[Array[Char]], char: Char) =
    (for {
      y <- heightmap.indices
      x <- heightmap(y).indices
      if heightmap(y)(x) == char
    } yield Pt(x, y)).head


  def canMove(heightmap: Array[Array[Char]], from: Pt, dest: Pt) = {
    val height = heightmap.length
    val width = heightmap(0).length
    def isWithinBoard(): Boolean = dest.x >= 0 && dest.y >= 0 && dest.x < width && dest.y < height

    def isMovable(): Boolean = {
      val fromVal = heightmap(from.y)(from.x);
      val destVal = heightmap(dest.y)(dest.x);
      (fromVal == 'S' && destVal == 'a') ||
        (if (destVal == 'E') fromVal == 'z' else fromVal + 1 >= destVal) // !!!!! ㅜㅜ
    }
    // (This also means that the elevation of the destination square can be much lower than the elevation of your current square.)
    isWithinBoard() && isMovable()
  }

  def printmap(dbgmap: mutable.Seq[ArrayBuffer[Int]]): Unit = {
    dbgmap.foreach(costs => {
      costs.foreach(cost => print("%3d,".format(cost)))
      println()
    })
  }

  def startAgain(heightmap: Array[Array[Char]]): Int = {
    val start = findPoint(heightmap, 'S')
    val end = findPoint(heightmap, 'E')

    val dbgmap = ArrayBuffer.fill(heightmap.length, heightmap(0).length)(0)  // DBG


    @tailrec
    def bfs(currents: Set[Pt], visited: Set[Pt] = Set.empty, cost: Int = 0): Int = {
      if (currents.contains(end)) {
        printmap(dbgmap) // DBG
        return cost
      }
      val newVisited = visited ++ currents

      currents.foreach(pt => dbgmap(pt.y)(pt.x) = cost) // DBG

      val nextPoints = currents.flatMap(pt => {
        Seq(-1, 1).flatMap(n => Seq(Pt(n, 0), Pt(0, n)).map(pt + _))
          .filter(n => !newVisited.contains(n))
          .filter(n => canMove(heightmap, pt, n))
      })
      bfs(nextPoints, newVisited, cost + 1)
    }

    bfs(Set(start))
  }

  val part1: Int = startAgain(inputs.split("\n").map(str => str.toArray))
  println(part1)
}

case class Pt(x: Int, y: Int) {
  def +(that: Pt) = Pt(x + that.x, y + that.y)
}
