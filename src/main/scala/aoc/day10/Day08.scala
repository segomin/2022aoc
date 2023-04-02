package aoc

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App {
  val filename = "src/main/scala/aoc/day10/Day08.input"
  val lines = Source.fromFile(filename).getLines()

  val inputs = lines.toArray
  val board = Board.of(inputs)
  val mid = Point(board.width / 2, board.height / 2)

  board.searchFrom(mid)
  val part1 = board.countAllVisible()
  println(part1)

  val countBoard = Board.of(inputs)
  countBoard.countFrom(mid)
  val part2 = countBoard.getMaxVisibleTreeCount()
  println(part2)
}


case class Point(x: Int, y: Int) {
  def isEdge(width: Int, height: Int): Boolean = x == 0 || y == 0 || x == width - 1 || y == height - 1
  def isOutRange(width: Int, height: Int): Boolean = x < 0 || y < 0 || x > width - 1 || y > height - 1
  def left(): Point = Point(x - 1, y)
  def right(): Point = Point(x + 1, y)
  def up(): Point = Point(x, y - 1)
  def down(): Point = Point(x, y + 1)
}


class Board(val board: Array[Array[Int]]) {
  final val MARK_INIT = -1
  final val MARK_VISIBLE = 1
  final val MARK_INVISIBLE = 0

  val width = board(0).length
  val height = board.length
  val markBoard = Array.tabulate(height, width)((_, _) => MARK_INIT)

  def isMarked(point: Point): Boolean = markBoard(point.y)(point.x) != MARK_INIT

  def markVisible(point: Point) = {
    markBoard(point.y)(point.x) = MARK_VISIBLE
  }

  def markInvisible(point: Point) = {
    markBoard(point.y)(point.x) = MARK_INVISIBLE
  }

  final def searchFrom(point: Point): Unit = {
    if (isMarked(point)) return

    if (point.isEdge(width, height)) {
      markVisible(point)
      return
    }

    if (isVisible(point)) {
      markVisible(point)
    } else {
      markInvisible(point)
    }

    searchFrom(point.left())
    searchFrom(point.right())
    searchFrom(point.up())
    searchFrom(point.down())
  }

  final def countFrom(point: Point): Unit = {
    if (point.isOutRange(width, height)) return

    if (isMarked(point)) return

    val count = countVisibleTree(point)
    markBoard(point.y)(point.x) = count

    countFrom(point.left())
    countFrom(point.right())
    countFrom(point.up())
    countFrom(point.down())
  }

  def isVisible(point: Point): Boolean = {
    val tree = getTreeOf(point)
    isVisibleOf(tree, point, _.left()) ||
      isVisibleOf(tree, point, _.right()) ||
      isVisibleOf(tree, point, _.up()) ||
      isVisibleOf(tree, point, _.down())
  }

  def countVisibleTree(point: Point): Int = {
    val tree = getTreeOf(point)
    countVisibleOf(tree, point, _.left(), 0) *
      countVisibleOf(tree, point, _.right(), 0) *
      countVisibleOf(tree, point, _.up(), 0) *
      countVisibleOf(tree, point, _.down(), 0)
  }

  def getTreeOf(point: Point): Int = board(point.y)(point.x)

  @tailrec
  final def isVisibleOf(tree: Int, prev: Point, func: Point => Point): Boolean = {
    val point = func(prev)
    if (prev.isEdge(width, height)) {
      true
    } else if (tree <= getTreeOf(point)) {
      false
    } else {
      isVisibleOf(tree, point, func)
    }
  }

  @tailrec
  final def countVisibleOf(tree: Int, prev: Point, func: Point => Point, dist: Int): Int = {
    val point = func(prev)
    if (prev.isEdge(width, height)) {
      dist
    } else if (tree <= getTreeOf(point)) {
      dist + 1
    } else {
      countVisibleOf(tree, point, func, dist + 1)
    }
  }

  def countAllVisible(): Int = markBoard.flatten.count(mark => mark != MARK_INVISIBLE)

  def getMaxVisibleTreeCount(): Int = markBoard.flatten.max
}

object Board {
  def of(inputs: Array[String]): Board = {
    val board = inputs.map(_.split("").map(_.toInt));
    new Board(board)
  }
}
